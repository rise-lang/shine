package shine.C.CodeGeneration

import arithexpr.arithmetic.BoolExpr.ArithPredicate
import arithexpr.arithmetic.{NamedVar, _}
import shine.C.AST.Block
import shine.DPIA.Compilation.SimplifyNats
import shine.DPIA.DSL._
import shine.DPIA.FunctionalPrimitives._
import shine.DPIA.ImperativePrimitives._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._
import shine._

import scala.collection.immutable.VectorBuilder
import scala.collection.{immutable, mutable}
import scala.language.implicitConversions

object CodeGenerator {

  final case class Environment(
      identEnv: immutable.Map[Identifier[_ <: BasePhraseTypes], C.AST.DeclRef],
      commEnv: immutable.Map[Identifier[CommType], C.AST.Stmt],
      contEnv: immutable.Map[Identifier[ExpType ->: CommType], Phrase[
        ExpType
      ] => Environment => C.AST.Stmt],
      letNatEnv: immutable.Map[LetNatIdentifier, Phrase[PhraseType]]
  ) {
    def updatedIdentEnv(
        kv: (Identifier[_ <: BasePhraseTypes], C.AST.DeclRef)
    ): Environment = {
      this.copy(identEnv = identEnv + kv)
    }

    def updatedCommEnv(kv: (Identifier[CommType], C.AST.Stmt)): Environment = {
      this.copy(commEnv = commEnv + kv)
    }

    def updatedContEnv(
        kv: (
            Identifier[ExpType ->: CommType],
            Phrase[ExpType] => Environment => C.AST.Stmt
        )
    ): Environment = {
      this.copy(contEnv = contEnv + kv)
    }

    def updatedNatEnv(
        kv: (LetNatIdentifier, Phrase[PhraseType])
    ): Environment = {
      this.copy(letNatEnv = this.letNatEnv + kv)
    }
  }

  object Environment {
    def empty = Environment(
      immutable.Map(),
      immutable.Map(),
      immutable.Map(),
      immutable.Map()
    )
  }

  sealed trait PathExpr
  sealed trait PairAccess extends PathExpr
  final case object FstMember extends PairAccess
  final case object SndMember extends PairAccess
  final case class CIntExpr(num: Nat) extends PathExpr
  implicit def cIntExprToNat(cexpr: CIntExpr): Nat = cexpr.num

  type Path = immutable.List[PathExpr]

  type Declarations = mutable.ListBuffer[C.AST.Decl]
  type Ranges = immutable.Map[String, arithexpr.arithmetic.Range]

  def apply(): CodeGenerator =
    new CodeGenerator(
      mutable.ListBuffer[C.AST.Decl](),
      immutable.Map[String, arithexpr.arithmetic.Range]()
    )
}

class CodeGenerator(
    val decls: CodeGenerator.Declarations,
    val ranges: CodeGenerator.Ranges
) extends DPIA.Compilation.CodeGenerator[
      CodeGenerator.Environment,
      CodeGenerator.Path,
      C.AST.Stmt,
      C.AST.Expr,
      C.AST.Decl,
      C.AST.DeclRef,
      C.AST.Type
    ] {

  import CodeGenerator._

  type Environment = CodeGenerator.Environment
  type Path = CodeGenerator.Path

  type Stmt = C.AST.Stmt
  type Decl = C.AST.Decl
  type Expr = C.AST.Expr
  type Ident = C.AST.DeclRef
  type Type = C.AST.Type

  override def name: String = "C"

  def addDeclaration(decl: Decl): Unit = {
    if (decls.exists(_.name == decl.name)) {
      println(s"warning: declaration with name ${decl.name} already defined")
    } else {
      decls += decl
    }
  }

  def updatedRanges(
      key: String,
      value: arithexpr.arithmetic.Range
  ): CodeGenerator =
    new CodeGenerator(decls, ranges.updated(key, value))

  override def generate(
      phrase: Phrase[CommType],
      topLevelDefinitions: scala.Seq[(LetNatIdentifier, Phrase[ExpType])],
      env: CodeGenerator.Environment
  ): (scala.Seq[Decl], Stmt) = {
    val stmt = this.generateWithFunctions(phrase, topLevelDefinitions, env)
    (decls, stmt)
  }

  def generateWithFunctions(
      phrase: Phrase[CommType],
      topLevelDefinitions: scala.Seq[(LetNatIdentifier, Phrase[ExpType])],
      env: CodeGenerator.Environment
  ): Stmt = {
    topLevelDefinitions.headOption match {
      case Some((ident, defn)) =>
        generateLetNat(
          ident,
          defn,
          env,
          (gen, env) =>
            gen.generateWithFunctions(phrase, topLevelDefinitions.tail, env)
        )
      case None => cmd(phrase, env)
    }
  }

  override def cmd(phrase: Phrase[CommType], env: Environment): Stmt = {
    visitAndGenerateNat(
      phrase match {
        case Phrases.IfThenElse(cond, thenP, elseP) =>
          exp(
            cond,
            env,
            Nil,
            cond =>
              C.AST.IfThenElse(cond, cmd(thenP, env), Some(cmd(elseP, env)))
          )

        case i: Identifier[CommType] => env.commEnv(i)

        case Apply(i: Identifier[_], e) => // TODO: think about this
          env.contEnv(
            i.asInstanceOf[Identifier[ExpType ->: CommType]]
          )(
            e.asInstanceOf[Phrase[ExpType]]
          )(env)

        case Skip() => C.AST.Comment("skip")

        case Seq(p1, p2) => C.AST.Stmts(cmd(p1, env), cmd(p2, env))

        case Assign(_, a, e) =>
          exp(
            e,
            env,
            Nil,
            e => acc(a, env, Nil, a => C.AST.ExprStmt(C.AST.Assignment(a, e)))
          )

        case New(dt, Lambda(v, p)) => CCodeGen.codeGenNew(dt, v, p, env)

        case NewDoubleBuffer(_, _, dt, n, in, out, Lambda(ps, p)) =>
          CCodeGen.codeGenNewDoubleBuffer(ArrayType(n, dt), in, out, ps, p, env)

        case For(n, Lambda(i, p), unroll) =>
          CCodeGen.codeGenFor(n, i, p, unroll, env)

        case ForNat(n, DepLambda(i: NatIdentifier, p), unroll) =>
          CCodeGen.codeGenForNat(n, i, p, unroll, env)

        case Proj1(pair) => cmd(Lifting.liftPair(pair)._1, env)
        case Proj2(pair) => cmd(Lifting.liftPair(pair)._2, env)

        case LetNat(binder, defn, body) =>
          generateLetNat(binder, defn, env, (gen, env) => gen.cmd(body, env))

        case Comment(comment) => C.AST.Comment(comment)

        case Apply(_, _) | DepApply(_, _) | _: CommandPrimitive =>
          error(s"Don't know how to generate code for $phrase")
      },
      env
    )
  }

  override def acc(
      phrase: Phrase[AccType],
      env: Environment,
      path: Path,
      cont: Expr => Stmt
  ): Stmt = {
    phrase match {
      case i @ Identifier(_, AccType(dt)) =>
        cont(generateAccess(dt, env.identEnv.applyOrElse(i, (_: Phrase[_]) => {
          throw new Exception(
            s"Expected to find `$i' in the environment: `${env.identEnv}'"
          )
        }), path, env))

      case SplitAcc(n, _, _, a) =>
        path match {
          case (i: CIntExpr) :: ps =>
            acc(a, env, CIntExpr(i / n) :: CIntExpr(i % n) :: ps, cont)
          case _ => error(s"Expected a C-Integer-Expression on the path.")
        }
      case JoinAcc(_, m, _, a) =>
        path match {
          case (i: CIntExpr) :: (j: CIntExpr) :: ps =>
            acc(a, env, CIntExpr(i * m + j) :: ps, cont)
          case _ => error(s"Expected two C-Integer-Expressions on the path.")
        }
      case depJ @ DepJoinAcc(_, _, _, a) =>
        path match {
          case (i: CIntExpr) :: (j: CIntExpr) :: ps =>
            acc(
              a,
              env,
              CIntExpr(BigSum(0, i - 1, x => depJ.lenF(x)) + j) :: ps,
              cont
            )
          case _ => error(s"Expected two C-Integer-Expressions on the path.")
        }

      case TransposeAcc(_, _, _, a) =>
        path match {
          case (i: CIntExpr) :: (j: CIntExpr) :: ps =>
            acc(a, env, j :: i :: ps, cont)
          case _ => error(s"did not expect $path")
        }

      case PairAcc1(_, _, a) => acc(a, env, FstMember :: path, cont)
      case PairAcc2(_, _, a) => acc(a, env, SndMember :: path, cont)

      case ZipAcc1(_, _, _, a) =>
        path match {
          case (i: CIntExpr) :: ps => acc(a, env, i :: FstMember :: ps, cont)
          case _                   => error(s"Expected a C-Integer-Expression on the path.")
        }
      case ZipAcc2(_, _, _, a) =>
        path match {
          case (i: CIntExpr) :: ps => acc(a, env, i :: SndMember :: ps, cont)
          case _                   => error(s"Expected a C-Integer-Expression on the path.")
        }
      case UnzipAcc(_, _, _, a) =>
        path match {
          case (i: CIntExpr) :: FstMember :: ps =>
            acc(a, env, FstMember :: i :: ps, cont)
          case (i: CIntExpr) :: SndMember :: ps =>
            acc(a, env, SndMember :: i :: ps, cont)
          case _ => error(s"unexpected $path")
        }

      case TakeAcc(_, _, _, a) => acc(a, env, path, cont)
      case DropAcc(n, _, _, a) =>
        path match {
          case (i: CIntExpr) :: ps => acc(a, env, CIntExpr(i + n) :: ps, cont)
          case _                   => error(s"Expected a C-Integer-Expression on the path.")
        }

      case CycleAcc(_, m, _, a) =>
        path match {
          case (i: CIntExpr) :: ps => acc(a, env, CIntExpr(i % m) :: ps, cont)
          case _                   => error(s"Expected a C-Integer-Expression on the path.")
        }

      case ReorderAcc(n, _, idxF, a) =>
        path match {
          case (i: CIntExpr) :: ps =>
            acc(
              a,
              env,
              CIntExpr(
                OperationalSemantics.evalIndexExp(idxF(AsIndex(n, Natural(i))))
              ) :: ps,
              cont
            )
          case _ => error(s"Expected a C-Integer-Expression on the path.")
        }

      case MapAcc(n, dt, _, f, a) =>
        path match {
          case (i: CIntExpr) :: ps =>
            acc(f(IdxAcc(n, dt, AsIndex(n, Natural(i)), a)), env, ps, cont)
          case _ => error(s"Expected a C-Integer-Expression on the path.")
        }
      case MapFstAcc(_, dt2, dt3, f, a) =>
        path match {
          case FstMember :: ps => acc(f(PairAcc1(dt3, dt2, a)), env, ps, cont)
          case SndMember :: ps => acc(PairAcc2(dt3, dt2, a), env, ps, cont)
          case _               => error(s"unexpected $path")
        }
      case MapSndAcc(dt1, _, dt3, f, a) =>
        path match {
          case FstMember :: ps => acc(PairAcc1(dt1, dt3, a), env, ps, cont)
          case SndMember :: ps => acc(f(PairAcc2(dt1, dt3, a)), env, ps, cont)
          case _               => error(s"unexpected $path")
        }

      case IdxAcc(_, _, i, a) => CCodeGen.codeGenIdxAcc(i, a, env, path, cont)

      case DepIdxAcc(_, _, i, a) => acc(a, env, CIntExpr(i) :: path, cont)

      case Proj1(pair) => acc(Lifting.liftPair(pair)._1, env, path, cont)
      case Proj2(pair) => acc(Lifting.liftPair(pair)._2, env, path, cont)

      case Apply(_, _) | DepApply(_, _) | Phrases.IfThenElse(_, _, _) |
          LetNat(_, _, _) | _: AccPrimitive =>
        error(s"Don't know how to generate code for $phrase")
    }
  }

  override def exp(
      phrase: Phrase[ExpType],
      env: Environment,
      path: Path,
      cont: Expr => Stmt
  ): Stmt = {
    phrase match {
      case i @ Identifier(_, ExpType(dt, _)) =>
        cont(generateAccess(dt, env.identEnv.applyOrElse(i, (_: Phrase[_]) => {
          throw new Exception(
            s"Expected to find `$i' in the environment: `${env.identEnv}'"
          )
        }), path, env))

      case Phrases.Literal(n) =>
        cont(path match {
          case Nil =>
            n.dataType match {
              case _: IndexType  => CCodeGen.codeGenLiteral(n)
              case _: ScalarType => CCodeGen.codeGenLiteral(n)
              case _             => error("Expected an IndexType or ScalarType.")
            }
          case (i: CIntExpr) :: ps =>
            (n, n.dataType) match {
              case (ArrayData(elems), ArrayType(_, et)) =>
                try {
                  generateAccess(
                    et,
                    CCodeGen.codeGenLiteral(elems(i.eval)),
                    ps,
                    env
                  )
                } catch {
                  case NotEvaluableException() =>
                    error(s"could not evaluate $i")
                }
              case _ => error("Expected an ArrayType.")
            }
          case _ => error(s"Unexpected: $n $path")
        })

      case Phrases.Natural(n) =>
        cont(path match {
          case Nil => C.AST.ArithmeticExpr(n)
          case _   => error(s"Expected the path to be empty.")
        })

      case uop @ UnaryOp(op, e) =>
        uop.t.dataType match {
          case _: ScalarType =>
            path match {
              case Nil =>
                exp(e, env, Nil, e => cont(CCodeGen.codeGenUnaryOp(op, e)))
              case _ => error(s"Expected path to be empty")
            }
          case _ => error(s"Expected scalar types")
        }

      case bop @ BinOp(op, e1, e2) =>
        bop.t.dataType match {
          case _: ScalarType =>
            path match {
              case Nil =>
                exp(
                  e1,
                  env,
                  Nil,
                  e1 =>
                    exp(
                      e2,
                      env,
                      Nil,
                      e2 => cont(CCodeGen.codeGenBinaryOp(op, e1, e2))
                    )
                )
              case _ => error(s"Expected path to be empty")
            }
          case _ => error(s"Expected scalar types, but ${bop.t.dataType} found")
        }

      case Cast(_, dt, e) =>
        path match {
          case Nil =>
            exp(e, env, Nil, e => cont(C.AST.Cast(typ(dt), e)))
          case _ => error(s"Expected path to be empty")
        }

      case IndexAsNat(_, e) => exp(e, env, path, cont)

      case AsIndex(_, e) => exp(e, env, path, cont)

      case Split(n, _, _, _, e) =>
        path match {
          case (i: CIntExpr) :: (j: CIntExpr) :: ps =>
            exp(e, env, CIntExpr(n * i + j) :: ps, cont)
          case _ => error(s"Expected two C-Integer-Expressions on the path.")
        }
      case Join(_, m, _, _, e) =>
        path match {
          case (i: CIntExpr) :: ps =>
            exp(e, env, CIntExpr(i / m) :: CIntExpr(i % m) :: ps, cont)
          case _ => error(s"Expected two C-Integer-Expressions on the path.")
        }

      case part @ Partition(_, _, _, _, e) =>
        path match {
          case (i: CIntExpr) :: (j: CIntExpr) :: ps =>
            exp(
              e,
              env,
              CIntExpr(BigSum(0, i - 1, x => part.lenF(x)) + j) :: ps,
              cont
            )
          case _ => error(s"Expected path to contain at least two elements")
        }

      case Zip(n, dt1, dt2, e1, e2) =>
        path match {
          case (i: CIntExpr) :: (xj: PairAccess) :: ps =>
            xj match {
              case FstMember => exp(e1, env, i :: ps, cont)
              case SndMember => exp(e2, env, i :: ps, cont)
            }
          case (i: CIntExpr) :: Nil =>
            val j = AsIndex(n, Natural(i))
            exp(
              Pair(dt1, dt2, Idx(n, dt1, j, e1), Idx(n, dt2, j, e2)),
              env,
              Nil,
              cont
            )
          case _ => error(s"unexpected $path")
        }

      case Unzip(_, _, _, e) =>
        path match {
          case (xj: PairAccess) :: (i: CIntExpr) :: ps =>
            exp(e, env, i :: xj :: ps, cont)
          case _ =>
            error(
              "Expected a tuple access followed by a C-Integer-Expression on the path."
            )
        }

      case DepZip(_, _, _, e1, e2) =>
        path match {
          case (i: CIntExpr) :: (xj: PairAccess) :: ps =>
            xj match {
              case FstMember => exp(e1, env, i :: ps, cont)
              case SndMember => exp(e2, env, i :: ps, cont)
            }
          case _ =>
            error(
              "Expected a C-Integer-Expression followed by a tuple access on the path."
            )
        }

      case r @ Pair(_, _, e1, e2) =>
        path match {
          case (xj: PairAccess) :: ps =>
            xj match {
              case FstMember => exp(e1, env, ps, cont)
              case SndMember => exp(e2, env, ps, cont)
            }
          case Nil =>
            exp(
              e1,
              env,
              Nil,
              ec1 =>
                exp(
                  e2,
                  env,
                  Nil,
                  ec2 => cont(C.AST.RecordLiteral(typ(r.t.dataType), ec1, ec2))
                )
            )
          case _ => error(s"unexpected $path")
        }
      case Fst(_, _, e) => exp(e, env, FstMember :: path, cont)
      case Snd(_, _, e) => exp(e, env, SndMember :: path, cont)

      case Take(_, _, _, _, e) => exp(e, env, path, cont)

      case Drop(n, _, _, _, e) =>
        path match {
          case (i: CIntExpr) :: ps => exp(e, env, CIntExpr(i + n) :: ps, cont)
          case _                   => error(s"Expected a C-Integer-Expression on the path.")
        }

      case Cycle(_, m, _, e) =>
        path match {
          case (i: CIntExpr) :: ps => exp(e, env, CIntExpr(i % m) :: ps, cont)
          case _                   => error(s"Expected a C-Integer-Expression on the path.")
        }

      case Reorder(n, _, idxF, _, a) =>
        path match {
          case (i: CIntExpr) :: ps =>
            exp(
              a,
              env,
              CIntExpr(
                OperationalSemantics.evalIndexExp(idxF(AsIndex(n, Natural(i))))
              ) :: ps,
              cont
            )
          case _ => error(s"Expected a C-Integer-Expression on the path.")
        }

      case Gather(n, m, dt, y, e) =>
        path match {
          case (i: CIntExpr) :: ps =>
            val yi = Idx(m, IndexType(n), AsIndex(m, Natural(i)), y)
            exp(Idx(n, dt, yi, e), env, ps, cont)
          case _ => error(s"unexpected $path")
        }

      case PadClamp(n, l, r, _, e) =>
        path match {
          case (i: CIntExpr) :: ps =>
            exp(
              e,
              env,
              CIntExpr(0) :: ps,
              left =>
                exp(
                  e,
                  env,
                  CIntExpr(n - 1) :: ps,
                  right => genPad(n, l, r, left, right, i, ps, e, env, cont)
                )
            )
          case _ => error(s"Expected path to be not empty")
        }

      case Pad(n, l, r, _, pad, array) =>
        path match {
          case (i: CIntExpr) :: ps =>
            exp(
              pad,
              env,
              ps,
              padExpr =>
                genPad(n, l, r, padExpr, padExpr, i, ps, array, env, cont)
            )

          case _ => error(s"Expected path to be not empty")
        }

      case Slide(_, _, s2, _, e) =>
        path match {
          case (i: CIntExpr) :: (j: CIntExpr) :: ps =>
            exp(e, env, CIntExpr(i * s2 + j) :: ps, cont)
          case _ => error(s"Expected two C-Integer-Expressions on the path.")
        }

      case Transpose(_, _, _, e) =>
        path match {
          case (i: CIntExpr) :: (j: CIntExpr) :: ps =>
            exp(e, env, j :: i :: ps, cont)
          case _ => error(s"did not expect $path")
        }

      case TransposeDepArray(_, _, _, e) =>
        path match {
          case (i: CIntExpr) :: (j: CIntExpr) :: ps =>
            exp(e, env, CIntExpr(j) :: CIntExpr(i) :: ps, cont)
          case _ => error(s"Expected two C-Integer-Expressions on the path.")
        }

      case Map(n, dt, _, f, e) =>
        path match {
          case (i: CIntExpr) :: ps =>
            exp(f(Idx(n, dt, AsIndex(n, Natural(i)), e)), env, ps, cont)
          case _ => error(s"Expected a C-Integer-Expression on the path.")
        }

      // TODO: we could get rid of that
      case MapRead(n, dt1, dt2, f, e) =>
        path match {
          case (i: CIntExpr) :: ps =>
            val continue_cmd =
              Identifier[ExpType ->: CommType](
                s"continue_$freshName",
                ExpType(dt2, read) ->: comm
              )

            cmd(
              f(
                Idx(n, dt1, AsIndex(n, Natural(i)), e)
              )(
                continue_cmd
              ),
              env updatedContEnv (continue_cmd -> (
                  e => env => exp(e, env, ps, cont)
              ))
            )
          case _ => error(s"Expected path to be not empty")
        }

      case GenerateCont(n, dt, f) =>
        path match {
          case (i: CIntExpr) :: ps =>
            val continue_cmd =
              Identifier[ExpType ->: CommType](
                s"continue_$freshName",
                ExpType(dt, read) ->: comm
              )

            cmd(
              f(AsIndex(n, Natural(i)))(continue_cmd),
              env updatedContEnv (continue_cmd -> (
                  e => env => exp(e, env, ps, cont)
              ))
            )
          case _ => error(s"Expected path to be not empty")
        }

      case Array(_, elems) =>
        path match {
          case (i: CIntExpr) :: ps =>
            try {
              exp(elems(i.eval), env, ps, cont)
            } catch {
              case NotEvaluableException() => error(s"could not evaluate $i")
            }
          case _ => error(s"did not expect $path")
        }

      case Idx(_, _, i, e) => CCodeGen.codeGenIdx(i, e, env, path, cont)

      case DepIdx(_, _, i, e) => exp(e, env, CIntExpr(i) :: path, cont)

      case ForeignFunction(f, inTs, outT, args) =>
        CCodeGen.codeGenForeignFunction(
          f,
          inTs,
          outT,
          args,
          env,
          path,
          fe => cont(generateAccess(outT, fe, path, env))
        )

      case Proj1(pair) =>
        exp(
          SimplifyNats.simplifyIndexAndNatExp(Lifting.liftPair(pair)._1),
          env,
          path,
          cont
        )
      case Proj2(pair) =>
        exp(
          SimplifyNats.simplifyIndexAndNatExp(Lifting.liftPair(pair)._2),
          env,
          path,
          cont
        )

      case Apply(_, _) | DepApply(_, _) | Phrases.IfThenElse(_, _, _) |
          LetNat(_, _, _) | _: ExpPrimitive =>
        error(s"Don't know how to generate code for $phrase")
    }
  }

  override def typ(dt: DataType): Type = {
    def typeToStructNameComponent(t: DataType): String = {
      t match {
        case IndexType(n)    => s"idx$n"
        case ArrayType(n, t) => s"${n}_${typeToStructNameComponent(t)}"
        case PairType(a, b) =>
          s"_${typeToStructNameComponent(a)}_${typeToStructNameComponent(b)}_"
        case _: BasicType => typ(t).toString
        case _            => ???
      }
    }

    dt match {
      case b: shine.DPIA.Types.BasicType =>
        b match {
          case shine.DPIA.Types.bool                           => C.AST.Type.int
          case shine.DPIA.Types.int | shine.DPIA.Types.NatType => C.AST.Type.int
          case shine.DPIA.Types.`u8`                           => C.AST.Type.u8
          case shine.DPIA.Types.`u16`                          => C.AST.Type.u16
          case shine.DPIA.Types.`u32`                          => C.AST.Type.u32
          case shine.DPIA.Types.`u64`                          => C.AST.Type.u64
          case shine.DPIA.Types.`i8`                           => C.AST.Type.i8
          case shine.DPIA.Types.`i16`                          => C.AST.Type.i16
          case shine.DPIA.Types.`i32`                          => C.AST.Type.i32
          case shine.DPIA.Types.`i64`                          => C.AST.Type.i64
          case shine.DPIA.Types.`f16`                          => ???
          case shine.DPIA.Types.`f32`                          => C.AST.Type.float
          case shine.DPIA.Types.`f64`                          => C.AST.Type.double
          case _: shine.DPIA.Types.IndexType                   => C.AST.Type.int
          case _: shine.DPIA.Types.VectorType =>
            throw new Exception("Vector types in C are not supported")
        }
      case a: shine.DPIA.Types.ArrayType =>
        C.AST.ArrayType(typ(a.elemType), Some(a.size))
      case a: shine.DPIA.Types.DepArrayType =>
        a.elemFType match {
          case NatToDataLambda(_, body) =>
            C.AST.ArrayType(typ(body), Some(a.size)) // TODO: be more precise with the size?
          case _: NatToDataIdentifier =>
            throw new Exception("This should not happen")
        }
      case r: shine.DPIA.Types.PairType =>
        C.AST.StructType(
          "Record_" + typeToStructNameComponent(r.fst) + "_" + typeToStructNameComponent(
            r.snd
          ),
          immutable.Seq((typ(r.fst), "_fst"), (typ(r.snd), "_snd"))
        )
      case _: shine.DPIA.Types.DataTypeIdentifier =>
        throw new Exception("This should not happen")
      case _: shine.DPIA.Types.NatToDataApply =>
        throw new Exception("This should not happen")
    }
  }

  override def generateAccess(
      dt: DataType,
      expr: Expr,
      path: Path,
      env: Environment
  ): Expr = {
    path match {
      case Nil => expr
      case (xj: PairAccess) :: ps =>
        dt match {
          case rt: PairType =>
            val (structMember, dt2) = xj match {
              case FstMember => ("_fst", rt.fst)
              case SndMember => ("_snd", rt.snd)
            }
            generateAccess(
              dt2,
              C.AST.StructMemberAccess(expr, C.AST.DeclRef(structMember)),
              ps,
              env
            )
          case _ => throw new Exception("expected tuple type")
        }
      case (_: CIntExpr) :: _ =>
        dt match {
          case at: ArrayType =>
            val (dt2, k, ps) = CCodeGen.flattenArrayIndices(at, path)
            generateAccess(
              dt2,
              C.AST.ArraySubscript(expr, C.AST.ArithmeticExpr(k)),
              ps,
              env
            )

          case dat: DepArrayType =>
            val (dt2, k, ps) = CCodeGen.flattenArrayIndices(dat, path)
            generateAccess(
              dt2,
              C.AST.ArraySubscript(expr, C.AST.ArithmeticExpr(k)),
              ps,
              env
            )
          case x =>
            throw new Exception(
              s"Expected an ArrayType that is accessed by the index but found $x instead."
            )
        }
      case _ =>
        throw new Exception(
          s"Can't generate access for `$dt' with `${path.mkString("[", "::", "]")}'"
        )
    }
  }

  private def generateLetNat[T <: PhraseType](
      binder: LetNatIdentifier,
      defn: Phrase[T],
      env: Environment,
      cont: (CodeGenerator, Environment) => Stmt
  ): Stmt = {
    cont(
      this,
      env updatedNatEnv ((binder, defn.asInstanceOf[Phrase[PhraseType]]))
    )
  }

  /* Take a phrase representing a function (may have multiple level of lambas/DepLambda[NatKind, _]), and a series of arguments,
     and generates the code of the function applied to the arguments.
     Since the generated code is inlined directly, no special measures need to be taken to handle variables captured
     in the phrase
   */
  def generateInlinedCall[T <: PhraseType](
      phrase: Phrase[T],
      env: Environment,
      args: Iterable[Either[Phrase[ExpType], Nat]],
      cont: Expr => Stmt
  ): Stmt = {

    def error(s: String) =
      throw new Exception(s + " in deferred function generation")
    phrase match {
      case l: Lambda[ExpType, _] @unchecked =>
        args.headOption match {
          case Some(Right(_)) =>
            error("Nat argument passed but phrase type arg expected")
          case None => error("Parameter missing")
          case Some(Left(param)) =>
            generateInlinedCall(l(param), env, args.tail, cont)
        }
      case ndl: DepLambda[NatKind, _] @unchecked =>
        args.headOption match {
          case Some(Right(nat)) =>
            generateInlinedCall(ndl(nat), env, args.tail, cont)
          case None => error("Parameter missing")
          case Some(Left(_)) =>
            error("Expression phrase argument passed but nat expected")
        }
      case ep: Phrase[ExpType] @unchecked =>
        args.headOption match {
          case Some(_) => error("Too many arguments in deferred funct")
          case None =>
            exp(ep, env, List(), cont)
        }

      case _ => error(s"Cannot generate phrase $phrase")
    }
  }

  /**
    * This function takes a phrase with a nat dependent free variable `for`, and it generates a block
    * where `for` is bound to the arithmetic expression at.
    * @param `for` The free nat variable to substitute
    * @param phrase The phrase to generate
    * @param at The arithmetic expression we are generating phrase at
    * @param env Up-to-date environment
    * @return
    */
  protected def generateNatDependentBody(
      `for`: NatIdentifier,
      phrase: Phrase[CommType],
      at: ArithExpr,
      env: Environment
  ): Block = {
    PhraseType.substitute(at, `for`, in = phrase) |> (p => {
      val newIdentEnv = env.identEnv.map {
        case (Identifier(name, AccType(dt)), declRef) =>
          (
            Identifier(name, AccType(DataType.substitute(at, `for`, in = dt))),
            declRef
          )
        case (Identifier(name, ExpType(dt, a)), declRef) =>
          (
            Identifier(
              name,
              ExpType(DataType.substitute(at, `for`, in = dt), a)
            ),
            declRef
          )
        case x => x
      }
      C.AST.Block(immutable.Seq(this.cmd(p, env.copy(identEnv = newIdentEnv))))
    })
  }

  protected object CCodeGen {
    def codeGenNew(
        dt: DataType,
        v: Identifier[VarType],
        p: Phrase[CommType],
        env: Environment
    ): Stmt = {
      val ve = Identifier(s"${v.name}_e", v.t.t1)
      val va = Identifier(s"${v.name}_a", v.t.t2)
      val vC = C.AST.DeclRef(v.name)

      C.AST.Block(
        immutable.Seq(
          C.AST.DeclStmt(C.AST.VarDecl(vC.name, typ(dt))),
          cmd(
            Phrase.substitute(PhrasePair(ve, va), `for` = v, `in` = p),
            env updatedIdentEnv (ve -> vC)
              updatedIdentEnv (va -> vC)
          )
        )
      )
    }

    def codeGenNewDoubleBuffer(
        dt: ArrayType,
        in: Phrase[ExpType],
        out: Phrase[AccType],
        ps: Identifier[VarType x CommType x CommType],
        p: Phrase[CommType],
        env: Environment
    ): Stmt = {
      import C.AST._
      import BinaryOperator._
      import UnaryOperator._

      val ve = Identifier(s"${ps.name}_e", ps.t.t1.t1.t1)
      val va = Identifier(s"${ps.name}_a", ps.t.t1.t1.t2)
      val done = Identifier(s"${ps.name}_swap", ps.t.t1.t2)
      val swap = Identifier(s"${ps.name}_done", ps.t.t2)

      val tmp1 = DeclRef(freshName("tmp1_"))
      val tmp2 = DeclRef(freshName("tmp2_"))
      val in_ptr = DeclRef(freshName("in_ptr_"))
      val out_ptr = DeclRef(freshName("out_ptr_"))
      val flag = DeclRef(freshName("flag_"))

      Block(
        immutable.Seq(
          // create variables: `tmp1', `tmp2`, `in_ptr', and `out_ptr'
          DeclStmt(VarDecl(tmp1.name, typ(dt))),
          DeclStmt(VarDecl(tmp2.name, typ(dt))),
          exp(
            in,
            env,
            CIntExpr(0) :: Nil,
            e => makePointerDecl(in_ptr.name, dt.elemType, UnaryExpr(&, e))
          ),
          makePointerDecl(out_ptr.name, dt.elemType, tmp1),
          // create boolean flag used for swapping
          DeclStmt(VarDecl(flag.name, Type.uchar, Some(Literal("1")))),
          // generate body
          cmd(
            Phrase.substitute(
              PhrasePair(PhrasePair(PhrasePair(ve, va), swap), done),
              `for` = ps,
              `in` = p
            ),
            env updatedIdentEnv (ve -> in_ptr) updatedIdentEnv (va -> out_ptr)
              updatedCommEnv (swap -> {
                Block(
                  immutable.Seq(
                    ExprStmt(Assignment(in_ptr, TernaryExpr(flag, tmp1, tmp2))),
                    ExprStmt(
                      Assignment(out_ptr, TernaryExpr(flag, tmp2, tmp1))
                    ),
                    // toggle flag with xor
                    ExprStmt(
                      Assignment(flag, BinaryExpr(flag, ^, Literal("1")))
                    )
                  )
                )
              })
              updatedCommEnv (done -> {
                Block(
                  immutable.Seq(
                    ExprStmt(Assignment(in_ptr, TernaryExpr(flag, tmp1, tmp2))),
                    acc(
                      out,
                      env,
                      CIntExpr(0) :: Nil,
                      o => ExprStmt(Assignment(out_ptr, UnaryExpr(&, o)))
                    )
                  )
                )
              })
          )
        )
      )
    }

    def codeGenFor(
        n: Nat,
        i: Identifier[ExpType],
        p: Phrase[CommType],
        unroll: Boolean,
        env: Environment
    ): Stmt = {
      assert(!unroll)
      val cI = C.AST.DeclRef(freshName("i_"))
      val range = RangeAdd(0, n, 1)
      val updatedGen = updatedRanges(cI.name, range)

      applySubstitutions(n, env.identEnv) |> (n => {

        range.numVals match {
          // iteration count is 0 => skip body; no code to be emitted
          case Cst(0) => C.AST.Comment("iteration count is 0, no loop emitted")

          // iteration count is 1 => no loop
          case Cst(1) =>
            C.AST.Stmts(
              C.AST.Stmts(
                C.AST.Comment("iteration count is exactly 1, no loop emitted"),
                C.AST.DeclStmt(
                  C.AST.VarDecl(
                    cI.name,
                    C.AST.Type.int,
                    init = Some(C.AST.ArithmeticExpr(0))
                  )
                )
              ),
              updatedGen.cmd(p, env updatedIdentEnv (i -> cI))
            )

          case _ =>
            val init = C.AST.VarDecl(
              cI.name,
              C.AST.Type.int,
              init = Some(C.AST.ArithmeticExpr(0))
            )
            val cond = C.AST
              .BinaryExpr(cI, C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n))
            val increment = C.AST.Assignment(
              cI,
              C.AST.ArithmeticExpr(NamedVar(cI.name, range) + 1)
            )

            C.AST.ForLoop(
              C.AST.DeclStmt(init),
              cond,
              increment,
              C.AST.Block(
                immutable.Seq(updatedGen.cmd(p, env updatedIdentEnv (i -> cI)))
              )
            )
        }
      })
    }

    def codeGenForNat(
        n: Nat,
        i: NatIdentifier,
        p: Phrase[CommType],
        unroll: Boolean,
        env: Environment
    ): Stmt = {
      assert(!unroll)
      val cI = C.AST.DeclRef(freshName("i_"))
      val range = RangeAdd(0, n, 1)
      val updatedGen = updatedRanges(cI.name, range)

      applySubstitutions(n, env.identEnv) |> (n => {

        range.numVals match {
          // iteration count is 0 => skip body; no code to be emitted
          case Cst(0) => C.AST.Comment("iteration count is 0, no loop emitted")

          // iteration count is 1 => no loop
          case Cst(1) =>
            C.AST.Stmts(
              C.AST.Stmts(
                C.AST.Comment("iteration count is exactly 1, no loop emitted"),
                C.AST.DeclStmt(
                  C.AST.VarDecl(
                    cI.name,
                    C.AST.Type.int,
                    init = Some(C.AST.ArithmeticExpr(0))
                  )
                )
              ),
              updatedGen.cmd(p, env)
            )

          case _ =>
            val init = C.AST.VarDecl(
              cI.name,
              C.AST.Type.int,
              init = Some(C.AST.ArithmeticExpr(0))
            )
            val cond = C.AST
              .BinaryExpr(cI, C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n))
            val increment = C.AST.Assignment(
              cI,
              C.AST.ArithmeticExpr(NamedVar(cI.name, range) + 1)
            )

            C.AST.ForLoop(
              C.AST.DeclStmt(init),
              cond,
              increment,
              updatedGen.generateNatDependentBody(
                `for` = i,
                `phrase` = p,
                at = NamedVar(cI.name, range),
                env
              )
            )
        }
      })
    }

    def codeGenIdxAcc(i: Phrase[ExpType],
                      a: Phrase[AccType],
                      env: Environment,
                      ps: Path,
                      cont: Expr => Stmt): Stmt = {
      exp(i, env, Nil, {
        case C.AST.Literal(text) => acc(a, env, CIntExpr(Cst(text.toInt)) :: ps, cont)
        case C.AST.DeclRef(name) => acc(a, env, CIntExpr(NamedVar(name, ranges(name))) :: ps, cont)
        case C.AST.ArithmeticExpr(ae) => acc(a, env, CIntExpr(ae) :: ps, cont)
        case cExpr:C.AST.Expr =>
          val arithVar = NamedVar(freshName("idxAcc"))
          C.AST.Block(immutable.Seq(
            C.AST.DeclStmt(C.AST.VarDecl(arithVar.name, C.AST.Type.int, Some(cExpr))),
            acc(a, env, CIntExpr(arithVar) :: ps, cont)
          ))
      })
    }

    def codeGenLiteral(d: OperationalSemantics.Data): Expr = {
      d match {
        case NatData(n)      => C.AST.ArithmeticExpr(n)
        case IndexData(i, _) => C.AST.ArithmeticExpr(i)
        case _: IntData | _: FloatData | _: DoubleData | _: BoolData =>
          C.AST.Literal(d.toString)
        case ArrayData(a) =>
          d.dataType match {
            case ArrayType(_, ArrayType(_, _)) =>
              codeGenLiteral(
                ArrayData(a.flatten(d => d.asInstanceOf[ArrayData].a))
              )
            case _ =>
              C.AST.ArrayLiteral(
                typ(d.dataType).asInstanceOf[C.AST.ArrayType],
                a.map(codeGenLiteral)
              )
          }
        case PairData(fst, snd) =>
          C.AST.RecordLiteral(
            typ(d.dataType),
            codeGenLiteral(fst),
            codeGenLiteral(snd)
          )
        case VectorData(_) =>
          throw new Exception("VectorData not supported in C")
      }
    }

    def codeGenUnaryOp(op: Operators.Unary.Value, e: Expr): Expr = {
      C.AST.UnaryExpr(op, e)
    }

    def codeGenIdx(i: Phrase[ExpType],
                   e: Phrase[ExpType],
                   env: Environment,
                   ps: Path,
                   cont: Expr => Stmt): Stmt = {
      exp(i, env, Nil, {
        case C.AST.DeclRef(name) => exp(e, env, CIntExpr(NamedVar(name, ranges(name))) :: ps, cont)
        case C.AST.ArithmeticExpr(ae) => exp(e, env, CIntExpr(ae) :: ps, cont)
        case cExpr:C.AST.Expr =>
          val arithVar = NamedVar(freshName("idx"))
          C.AST.Block(immutable.Seq(
            C.AST.DeclStmt(C.AST.VarDecl(arithVar.name, C.AST.Type.int, Some(cExpr))),
            exp(e, env, CIntExpr(arithVar) :: ps, cont)
          ))
      })
    }

    def codeGenForeignFunction(
        funDecl: ForeignFunction.Declaration,
        inTs: collection.Seq[DataType],
        outT: DataType,
        args: collection.Seq[Phrase[ExpType]],
        env: Environment,
        ps: Path,
        cont: Expr => Stmt
    ): Stmt = {
      funDecl.definition match {
        case Some(funDef) =>
          addDeclaration(
            C.AST.FunDecl(
              funDecl.name,
              returnType = typ(outT),
              params = (funDef.params zip inTs).map {
                case (name, dt) => C.AST.ParamDecl(name, typ(dt))
              },
              body = C.AST.Code(funDef.body)
            )
          )
        case _ =>
      }

      codeGenForeignCall(funDecl.name, args, env, Nil, cont)
    }

    def codeGenForeignCall(
        name: String,
        args: collection.Seq[Phrase[ExpType]],
        env: Environment,
        args_ps: Path,
        cont: Expr => Stmt
    ): Stmt = {
      def iter(
          args: collection.Seq[Phrase[ExpType]],
          res: VectorBuilder[Expr]
      ): Stmt = {
        //noinspection VariablePatternShadow
        args match {
          case a +: args =>
            exp(a, env, args_ps, a => iter(args, res += a))
          case _ => cont(C.AST.FunCall(C.AST.DeclRef(name), res.result()))
        }
      }

      iter(args, new VectorBuilder())
    }

    def codeGenBinaryOp(
        op: Operators.Binary.Value,
        e1: Expr,
        e2: Expr
    ): Expr = {
      C.AST.BinaryExpr(e1, op, e2)
    }

    def flattenArrayIndices(dt: DataType, path: Path): (DataType, Nat, Path) = {
      assert(dt.isInstanceOf[ArrayType] || dt.isInstanceOf[DepArrayType])

      val (arrayLayers, dt2) = peelArrayLayers(dt)
      val (indicesAsPathElements, rest) = path.splitAt(arrayLayers)
      indicesAsPathElements.foreach(i => assert(i.isInstanceOf[CIntExpr]))
      val indices = indicesAsPathElements.map(_.asInstanceOf[CIntExpr].num)
      // vector indexing also uses CIntExpr
      // assert(rest.isEmpty || !rest.head.isInstanceOf[CIntExpr])

      val subMap = buildSubMap(dt, indices)

      (dt2, ArithExpr.substitute(flattenIndices(dt, indices), subMap), rest)
    }

    def peelArrayLayers(dataType: DataType): (Int, DataType) = {
      dataType match {
        case ArrayType(_, et) =>
          val (n, dt) = peelArrayLayers(et)
          (n + 1, dt)
        case DepArrayType(_, NatToDataLambda(_, et)) =>
          val (n, dt) = peelArrayLayers(et)
          (n + 1, dt)
        case _ => (0, dataType)
      }
    }

    def flattenIndices(dataType: DataType, indicies: List[Nat]): Nat = {
      (dataType, indicies) match {
        case (array: ArrayType, index :: rest) =>
          numberOfElementsUntil(array, index) + flattenIndices(
            array.elemType,
            rest
          )
        case (array: DepArrayType, index :: rest) =>
          array.elemFType match {
            case NatToDataLambda(_, body) =>
              numberOfElementsUntil(array, index) + flattenIndices(body, rest)
            case _: NatToDataIdentifier =>
              throw new Exception(s"This should not happen")
          }
        case (_, Nil) => 0
        case t        => throw new Exception(s"This should not happen, pair $t")
      }
    }

    //Computes the total number of element in an array at a given offset
    def numberOfElementsUntil(dt: ArrayType, at: Nat): Nat = {
      DataType.getTotalNumberOfElements(dt.elemType) * at
    }

    def numberOfElementsUntil(dt: DepArrayType, at: Nat): Nat = {
      dt.elemFType match {
        case NatToDataLambda(x, body) =>
          BigSum(
            from = 0,
            upTo = at - 1,
            `for` = x,
            DataType.getTotalNumberOfElements(body)
          )
        case _: NatToDataIdentifier =>
          throw new Exception(s"This should not happen")
      }
    }

    private def getIndexVariablesScopes(
        dt: DataType
    ): List[Option[NatIdentifier]] = {
      dt match {
        case ArrayType(_, et) => None :: getIndexVariablesScopes(et)
        case DepArrayType(_, NatToDataLambda(i, et)) =>
          Some(i) :: getIndexVariablesScopes(et)
        case _ => Nil
      }
    }

    private def buildSubMap(
        dt: DataType,
        indices: immutable.Seq[Nat]
    ): Predef.Map[Nat, Nat] = {
      val bindings = getIndexVariablesScopes(dt)
      bindings
        .zip(indices)
        .map({
          case (Some(binder), index) => Some((binder, index))
          case _                     => None
        })
        .filter(_.isDefined)
        .map(_.get)
        .toMap[Nat, Nat]
    }

    implicit def convertBinaryOp(
        op: Operators.Binary.Value
    ): shine.C.AST.BinaryOperator.Value = {
      import Operators.Binary._
      op match {
        case ADD => C.AST.BinaryOperator.+
        case SUB => C.AST.BinaryOperator.-
        case MUL => C.AST.BinaryOperator.*
        case DIV => C.AST.BinaryOperator./
        case MOD => C.AST.BinaryOperator.%
        case GT  => C.AST.BinaryOperator.>
        case LT  => C.AST.BinaryOperator.<
        case EQ  => C.AST.BinaryOperator.==
      }
    }

    implicit def convertUnaryOp(
        op: Operators.Unary.Value
    ): shine.C.AST.UnaryOperator.Value = {
      import Operators.Unary._
      op match {
        case NEG => C.AST.UnaryOperator.-
        case NOT => C.AST.UnaryOperator.!
      }
    }

    def makePointerDecl(name: String, elemType: DataType, expr: Expr): Stmt = {
      import C.AST._
      DeclStmt(VarDecl(name, PointerType(typ(elemType)), Some(expr)))
    }
  }

  protected def applySubstitutions(
      n: Nat,
      identEnv: immutable.Map[Identifier[_ <: BasePhraseTypes], C.AST.DeclRef]
  ): Nat = {
    // lift the substitutions from the Phrase level to the ArithExpr level
    val substitionMap = identEnv
      .filter(_._1.t match {
        case ExpType(IndexType(_), _) => true
        case AccType(IndexType(_))    => true
        case _                        => false
      })
      .map(i => (NamedVar(i._1.name), NamedVar(i._2.name)))
      .toMap[ArithExpr, ArithExpr]
    ArithExpr.substitute(n, substitionMap)
  }

  private def visitAndGenerateNat(node: Stmt, env: Environment): Stmt = {
    C.AST.Nodes.VisitAndGenerateStmt(
      node,
      new C.AST.Nodes.VisitAndGenerateStmt.Visitor() {
        override def onExpr(e: Expr, cont: Expr => Stmt): Stmt = e match {
          case C.AST.ArithmeticExpr(ae) => genNat(ae, env, cont)
          case other                    => cont(other)
        }
      }
    )
  }

  override def genNat(n: Nat, env: Environment, cont: Expr => Stmt): Stmt = {
    def boolExp(b: BoolExpr, env: Environment, cont: Expr => Stmt): Stmt =
      b match {
        case BoolExpr.True  => cont(C.AST.Literal("true"))
        case BoolExpr.False => cont(C.AST.Literal("false"))
        case BoolExpr.ArithPredicate(lhs, rhs, op) =>
          val cOp = op match {
            case ArithPredicate.Operator.!= => C.AST.BinaryOperator.!=
            case ArithPredicate.Operator.== => C.AST.BinaryOperator.==
            case ArithPredicate.Operator.<  => C.AST.BinaryOperator.<
            case ArithPredicate.Operator.<= => C.AST.BinaryOperator.<=
            case ArithPredicate.Operator.>  => C.AST.BinaryOperator.>
            case ArithPredicate.Operator.>= => C.AST.BinaryOperator.>=
          }
          genNat(
            lhs,
            env,
            lhs =>
              genNat(rhs, env, rhs => cont(C.AST.BinaryExpr(lhs, cOp, rhs)))
          )
      }

    /**
      * General form of expression generation algorithm from a series of nats,
      * Used by sum and product
      *
      *
      * @param nats The series of nats from which the expression is generated
      * @param op The binary operator used
      * @param default In case the nat list is empty, we shall return this value
      * @param cont The cont of the generation
      * @param accum Internal accumulator used across iterations
      * @return
      */
    def genBinopFold(
        nats: Iterable[Nat],
        op: C.AST.BinaryOperator.Value,
        default: Expr,
        cont: Expr => Stmt,
        accum: Option[Expr] = None
    ): Stmt = {
      nats.headOption match {
        case None => cont(accum.getOrElse(default))
        case Some(nat) =>
          accum match {
            case None =>
              genNat(
                nat,
                env,
                exp => genBinopFold(nats.tail, op, default, cont, Some(exp))
              )
            case Some(acc) =>
              genNat(
                nat,
                env,
                exp =>
                  genBinopFold(
                    nats.tail,
                    op,
                    default,
                    cont,
                    Some(C.AST.BinaryExpr(acc, op, exp))
                  )
              )
          }
      }
    }

    import C.AST
    n match {
      case Cst(c) => cont(AST.Literal(c.toString))
      case Pow(b, ex) =>
        ex match {
          case Cst(2) =>
            genNat(
              b,
              env,
              b => cont(AST.BinaryExpr(b, AST.BinaryOperator.*, b))
            )
          case _ =>
            // FIXME: this often generates functionally incorrect code
            genNat(
              b,
              env,
              b =>
                genNat(
                  ex,
                  env,
                  ex =>
                    cont(
                      AST.Cast(
                        AST.Type.int,
                        AST.FunCall(
                          AST.DeclRef("pow"),
                          immutable.Seq(
                            AST.Cast(AST.Type.float, b),
                            AST.Cast(AST.Type.float, ex)
                          )
                        )
                      )
                    )
                )
            )
        }
      case Log(b, x) =>
        genNat(
          b,
          env,
          b =>
            genNat(
              x,
              env,
              x =>
                cont(
                  AST.Cast(
                    AST.Type.int,
                    AST.FunCall(
                      AST.DeclRef("log" + b),
                      immutable.Seq(AST.Cast(AST.Type.float, x))
                    )
                  )
                )
            )
        )

      case Prod(es) =>
        var (num, denum) = es.partition({
          case Pow(_, Cst(-1)) => false
          case _               => true
        })
        denum = denum.map({ case Pow(b, Cst(-1)) => b })
        if (denum.nonEmpty) { // num /^ denum
          val aNum = num.fold(1: ArithExpr)(_ * _)
          val aDenum = denum.fold(1: ArithExpr)(_ * _)
          if (aNum % aDenum != Cst(0)) {
            println(s"WARNING: $aNum /^ $aDenum might have a fractional part")
          }
          genBinopFold(
            num,
            AST.BinaryOperator.*,
            AST.Literal("0"),
            lhs =>
              genBinopFold(
                denum,
                AST.BinaryOperator.*,
                AST.Literal("0"),
                rhs => cont(C.AST.BinaryExpr(lhs, C.AST.BinaryOperator./, rhs))
              )
          )
        } else {
          genBinopFold(es, AST.BinaryOperator.*, AST.Literal("0"), cont)
        }

      case Sum(es) =>
        genBinopFold(es, AST.BinaryOperator.+, AST.Literal("0"), cont)

      case Mod(a, n) =>
        if (arithexpr.arithmetic.ArithExpr.mightBeNegative(a)) {
          println(s"WARNING: $a % $n might operate on negative values")
        }
        genNat(
          a,
          env,
          a =>
            genNat(
              n,
              env,
              n => cont(AST.BinaryExpr(a, AST.BinaryOperator.%, n))
            )
        )

      case v: Var => cont(C.AST.DeclRef(v.toString))

      case IntDiv(n, d) =>
        genNat(
          n,
          env,
          n =>
            genNat(
              d,
              env,
              d => cont(AST.BinaryExpr(n, AST.BinaryOperator./, d))
            )
        )

      case LShift(a, b) =>
        genNat(
          a,
          env,
          a =>
            genNat(
              b,
              env,
              b => cont(AST.BinaryExpr(a, AST.BinaryOperator.<<, b))
            )
        )

      case lu: Lookup =>
        cont(
          AST.FunCall(
            AST.DeclRef(s"lookup${lu.id}"),
            immutable.Seq(AST.Literal(lu.index.toString))
          )
        )

      case arithexpr.arithmetic.IfThenElse(cond, trueBranch, falseBranch) =>
        boolExp(
          cond,
          env,
          cond =>
            genNat(
              trueBranch,
              env,
              trueBranch =>
                genNat(
                  falseBranch,
                  env,
                  falseBranch =>
                    cont(AST.TernaryExpr(cond, trueBranch, falseBranch))
                )
            )
        )

      case natFunCall: NatFunCall =>
        val phrase = env.letNatEnv(natFunCall.fun)

        val args = natFunCall.args.map({
          case NatArg(argN) => Right(argN)
          case LetNatIdArg(ident) =>
            val argPhrase = env.letNatEnv(ident)
            if (!argPhrase.t.isInstanceOf[ExpType]) {
              throw new Exception(
                "Cannot use non-expression let nat arguments in natFunCall"
              )
            }
            Left(argPhrase.asInstanceOf[Phrase[ExpType]])
        })

        visitAndGenerateNat(generateInlinedCall(phrase, env, args, cont), env)

      case sp: SteppedCase => genNat(sp.intoIfChain(), env, cont)

      case BigSum(variable, body) =>
        println(s"Generating for loop for big sum $n")
        genNat(
          variable.from,
          env,
          from => {
            genNat(
              variable.upTo,
              env,
              upTo => {
                genNat(
                  body,
                  env,
                  bodyE => {
                    val loopVar = C.AST.DeclRef(variable.toString)

                    val init = C.AST.DeclStmt(
                      C.AST.VarDecl(
                        loopVar.name,
                        C.AST.Type.int,
                        init = Some(from)
                      )
                    )
                    val cond =
                      C.AST.BinaryExpr(loopVar, C.AST.BinaryOperator.<, upTo)
                    val increment = C.AST.Assignment(
                      loopVar,
                      C.AST.ArithmeticExpr(
                        NamedVar(loopVar.name, variable.range) + 1
                      )
                    )

                    val accumVar = C.AST.VarDecl(
                      freshName("accum_"),
                      C.AST.Type.int,
                      Some(AST.Literal("0"))
                    )

                    val forLoop = C.AST.ForLoop(
                      init,
                      cond,
                      increment,
                      Block(
                        immutable.Seq(
                          C.AST.ExprStmt(
                            C.AST.Assignment(
                              C.AST.DeclRef(accumVar.name),
                              C.AST.BinaryExpr(
                                C.AST.DeclRef(accumVar.name),
                                C.AST.BinaryOperator.+,
                                bodyE
                              )
                            )
                          )
                        )
                      )
                    )

                    C.AST.Stmts(
                      C.AST.Stmts(
                        C.AST.DeclStmt(accumVar),
                        forLoop
                      ),
                      cont(C.AST.DeclRef(accumVar.name))
                    )
                  }
                )
              }
            )
          }
        )
      case otherwise =>
        throw new Exception(s"Don't know how to print $otherwise")
    }
  }

  protected def genPad(
      n: Nat,
      l: Nat,
      r: Nat,
      left: Expr,
      right: Expr,
      i: CIntExpr,
      ps: Path,
      array: Phrase[ExpType],
      env: Environment,
      cont: Expr => Stmt
  ): Stmt = {
    // FIXME: we should know that (i - l) is in [0; n[ here
    exp(
      array,
      env,
      CIntExpr(i - l) :: ps,
      arrayExpr => {

        def cOperator(
            op: ArithPredicate.Operator.Value
        ): C.AST.BinaryOperator.Value = op match {
          case ArithPredicate.Operator.<  => C.AST.BinaryOperator.<
          case ArithPredicate.Operator.>  => C.AST.BinaryOperator.>
          case ArithPredicate.Operator.>= => C.AST.BinaryOperator.>=
          case _                          => null
        }

        def genBranch(
            lhs: ArithExpr,
            rhs: ArithExpr,
            operator: ArithPredicate.Operator.Value,
            taken: Expr,
            notTaken: Expr
        ): Expr = {
          import BoolExpr._
          arithPredicate(lhs, rhs, operator) match {
            case True  => taken
            case False => notTaken
            case _ =>
              C.AST.TernaryExpr(
                C.AST.BinaryExpr(
                  C.AST.ArithmeticExpr(lhs),
                  cOperator(operator),
                  C.AST.ArithmeticExpr(rhs)
                ),
                taken,
                notTaken
              )
          }
        }
        cont(
          genBranch(
            i,
            l,
            ArithPredicate.Operator.<,
            left,
            genBranch(i, l + n, ArithPredicate.Operator.<, arrayExpr, right)
          )
        )
      }
    )
  }
}
