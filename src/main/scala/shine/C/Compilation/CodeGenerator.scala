package shine.C.Compilation

import arithexpr.arithmetic.BoolExpr.ArithPredicate
import arithexpr.arithmetic.{NamedVar, _}
import rise.core.types._
import rise.core.types.DataType._
import rise.core.substitute.{natInType => substituteNatInType, typeInType => substituteTypeInType}
import rise.core.types
import shine.C.AST
import shine.C.AST.Block
import shine.C.AST.Type.getBaseType
import shine.DPIA.Compilation.Passes.SimplifyNats
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.primitives.functional
import shine.DPIA.primitives.functional._
import shine.DPIA.primitives.imperative._
import shine.DPIA.{error, _}
import shine._

import scala.collection.immutable.VectorBuilder
import scala.collection.{immutable, mutable}
import scala.language.implicitConversions

object CodeGenerator {
  sealed trait PathExpr
  sealed trait PairAccess extends PathExpr
  final case object FstMember extends PairAccess
  final case object SndMember extends PairAccess
  final case class CIntExpr(num: Nat) extends PathExpr
  final case object DPairSnd extends PathExpr

  implicit def cIntExprToNat(cexpr: CIntExpr): Nat = cexpr.num

  type Declarations = mutable.ListBuffer[C.AST.Decl]
  type Ranges = immutable.Map[String, arithexpr.arithmetic.Range]

  def apply(): CodeGenerator =
    new CodeGenerator(mutable.ListBuffer[C.AST.Decl](), immutable.Map[String, arithexpr.arithmetic.Range]())
}

class CodeGenerator(val decls: CodeGenerator.Declarations,
                    val ranges: CodeGenerator.Ranges)
  extends DPIA.Compilation.CodeGenerator {

  import CodeGenerator._

  type Path = immutable.List[PathExpr]

  type Stmt = C.AST.Stmt
  type Decl = C.AST.Decl
  type Expr = C.AST.Expr
  type Ident = C.AST.DeclRef
  type Type = C.AST.Type

  override def name: String = "C"

  override def translationContext: Compilation.TranslationContext = new TranslationContext()

  def addDeclaration(decl: Decl): Unit = {
    if (decls.exists(_.name == decl.name)) {
      println(s"warning: declaration with name ${decl.name} already defined")
    } else {
      decls += decl
    }
  }


  def updatedRanges(key: String, value: arithexpr.arithmetic.Range): CodeGenerator =
    new CodeGenerator(decls, ranges.updated(key, value))

  override def generate(topLevelDefinitions: immutable.Seq[(LetNatIdentifier, Phrase[ExpType])],
                        env: Environment): Phrase[CommType] => (immutable.Seq[Decl], Stmt) = phrase => {
    val stmt = generateWithFunctions(topLevelDefinitions, env)(phrase)
    (decls.toSeq, stmt)
  }

  def generateWithFunctions(topLevelDefinitions: immutable.Seq[(LetNatIdentifier, Phrase[ExpType])],
                            env: Environment): Phrase[CommType] => Stmt = phrase => {
    topLevelDefinitions.headOption match {
      case Some((ident, defn)) =>
        generateLetNat(ident, defn, env, (gen, env) =>
          gen.generateWithFunctions(topLevelDefinitions.tail, env)(phrase))
      case None => phrase |> cmd(env)
    }
  }

  override def cmd(env: Environment): Phrase[CommType] => Stmt = phrase => {
    visitAndGenerateNat(phrase match {
      case Phrases.IfThenElse(cond, thenP, elseP) =>
        cond |> exp(env, Nil, cond =>
          C.AST.IfThenElse(cond, cmd(env)(thenP), Some(cmd(env)(elseP))))

      case i: Identifier[CommType] => env.commEnv(i)

      case Apply(i: Identifier[_], e) => // TODO: think about this
        env.contEnv(
          i.asInstanceOf[Identifier[ExpType ->: CommType]]
        )(
          e.asInstanceOf[Phrase[ExpType]]
        )(env)

      case Skip() => C.AST.Comment("skip")

      case Seq(p1, p2) => C.AST.Stmts(p1 |> cmd(env), p2 |> cmd(env))

      case Assign(_, a, e) =>
        e |> exp(env, Nil, e =>
          a |> acc(env, Nil, a =>
            C.AST.ExprStmt(C.AST.Assignment(a, e))))

      case New(dt, Lambda(v, p)) => CCodeGen.codeGenNew(dt, v, p, env)

      case NewDoubleBuffer(_, _, dt, n, in, out, Lambda(ps, p)) =>
        CCodeGen.codeGenNewDoubleBuffer(ArrayType(n, dt), in, out, ps, p, env)

      case f@For(unroll) =>
        f.loopBody match {
          case Lambda(i, p) =>
            CCodeGen.codeGenFor(f.n, i, p, unroll, env)
          case _ => throw new Exception("This should not happen")
        }

      case f@ForNat(unroll) =>
        f.loopBody match {
          case shine.DPIA.Phrases.DepLambda(NatKind, i, p) =>
            CCodeGen.codeGenForNat(f.n, i, p, unroll, env)
          case _ => throw new Exception("This should not happen")
        }

      case Proj1(pair) => Lifting.liftPair(pair)._1 |> cmd(env)
      case Proj2(pair) => Lifting.liftPair(pair)._2 |> cmd(env)

      case LetNat(binder, defn, body) => generateLetNat(binder, defn, env, (gen, env) => body |> gen.cmd(env))

      case Comment(comment) => C.AST.Comment(comment)

      // on the fly beta-reduction
      case Apply(fun, arg) => Lifting.liftFunction(fun).reducing(arg) |> cmd(env)
      case DepApply(kind, fun, arg) => arg match {
        case a: Nat =>
          Lifting.liftDependentFunction(fun.asInstanceOf[Phrase[`(nat)->:`[CommType]]])(a) |> cmd(env)
        case a: DataType =>
          Lifting.liftDependentFunction(fun.asInstanceOf[Phrase[`(nat)->:`[CommType]]])(a) |> cmd(env)
      }

      case DMatchI(x, inT, _, f, dPair) =>
        dPair |> exp(env, List(), input => {
          // Create a fresh nat identifier, this will be bound to the "actual nat value"
          val fstId = NatIdentifier(freshName("fstId"))
          val sndT = substituteNatInType(fstId, x, inT)
          val sndId = Identifier[ExpType](freshName("sndId"),  ExpType(sndT, `read`))
          val (sndCType, initT) = typ(sndT) match {
              case array:C.AST.ArrayType => (C.AST.PointerType(getBaseType(array)), (x:C.AST.Expr) => x)
              case t => (t, (x:C.AST.Expr) => C.AST.UnaryExpr(C.AST.UnaryOperator.*, x))
            }
          // Read off the first uint32_t as a nat
          C.AST.Block(immutable.Seq(
            C.AST.DeclStmt(
              C.AST.VarDecl(fstId.name, C.AST.Type.u32,
                Some(C.AST.ArraySubscript(C.AST.Cast(C.AST.PointerType(C.AST.Type.u32), input), C.AST.Literal("0"))))
            ),
            C.AST.DeclStmt(
              C.AST.VarDecl(sndId.name, sndCType,
                Some(initT(C.AST.Cast(sndCType, C.AST.BinaryExpr(input, C.AST.BinaryOperator.+, C.AST.Literal("4")))))
            )),
            f(fstId)(sndId) |> cmd(env.updatedIdentEnv((sndId, C.AST.DeclRef(sndId.name))))
          ))
        })

      case MkDPairFstI(fst, a) =>
        genNat(fst, env, fst => {
          a |> acc(env, List(), expr => C.AST.ExprStmt(C.AST.Assignment(
            C.AST.ArraySubscript(C.AST.Cast(C.AST.PointerType(C.AST.Type.u32), expr), C.AST.Literal("0")
            ) , fst)))
        })
      case Apply(_, _) | DepApply(_, _, _) |
           _: CommandPrimitive =>
        error(s"Don't know how to generate code for $phrase")
    }, env)
  }

  override def acc(env: Environment,
                   path: Path,
                   cont: Expr => Stmt): Phrase[AccType] => Stmt = {
    case i@Identifier(_, AccType(dt)) => generateAccess(dt,
      env.identEnv.applyOrElse(i, (_: Phrase[_]) => {
        throw new Exception(s"Expected to find `$i' in the environment: `${env.identEnv}'")
      }), path, env, cont)

    case SplitAcc(n, _, _, a) => path match {
      case (i: CIntExpr) :: ps => a |> acc(env, CIntExpr(i / n) :: CIntExpr(i % n) :: ps, cont)
      case _ => error(s"Expected a C-Integer-Expression on the path.")
    }
    case JoinAcc(_, m, _, a) => path match {
      case (i: CIntExpr) :: (j: CIntExpr) :: ps => a |> acc(env, CIntExpr(i * m + j) :: ps, cont)
      case _ => error(s"Expected two C-Integer-Expressions on the path.")
    }
    case depJ@DepJoinAcc(_, _, _, a) => path match {
      case (i: CIntExpr) :: (j: CIntExpr) :: ps =>
        a |> acc(env, CIntExpr(BigSum(0, i - 1, x => depJ.lenF(x)) + j) :: ps, cont)
      case _ => error(s"Expected two C-Integer-Expressions on the path.")
    }

    case TransposeAcc(_, _, _, a) => path match {
      case (i: CIntExpr) :: (j: CIntExpr) :: ps => a |> acc(env, j :: i :: ps, cont)
      case _ => error(s"did not expect $path")
    }

    case PairAcc1(_, _, a) => a |> acc(env, FstMember :: path, cont)
    case PairAcc2(_, _, a) => a |> acc(env, SndMember :: path, cont)
    case PairAcc(_, _, fst, snd) => path match {
      case FstMember :: ps => fst |> acc(env, ps, cont)
      case SndMember :: ps => snd |> acc(env, ps, cont)
      case Nil =>
        // FIXME: hacky
        fst |> acc(env, Nil, { case C.AST.StructMemberAccess(a1, _) =>
          snd |> acc(env, Nil, { case C.AST.StructMemberAccess(a2, _) =>
            assert(a1 == a2)
            cont(a1)
          })})
      case _ => error(s"did not expect $path")
    }

    case ZipAcc1(_, _, _, a) => path match {
      case (i: CIntExpr) :: ps => a |> acc(env, i :: FstMember :: ps, cont)
      case _ => error(s"Expected a C-Integer-Expression on the path.")
    }
    case ZipAcc2(_, _, _, a) => path match {
      case (i: CIntExpr) :: ps => a |> acc(env, i :: SndMember :: ps, cont)
      case _ => error(s"Expected a C-Integer-Expression on the path.")
    }
    case UnzipAcc(_, _, _, a) => path match {
      case (i: CIntExpr) :: FstMember :: ps => a |> acc(env, FstMember :: i :: ps, cont)
      case (i: CIntExpr) :: SndMember :: ps => a |> acc(env, SndMember :: i :: ps, cont)
      case _ => error(s"unexpected $path")
    }

    case TakeAcc(_, _, _, a) => a |> acc(env, path, cont)
    case DropAcc(n, _, _, a) => path match {
      case (i: CIntExpr) :: ps => a |> acc(env, CIntExpr(i + n) :: ps, cont)
      case _ => error(s"Expected a C-Integer-Expression on the path.")
    }

    case CycleAcc(_, m, _, a) => path match {
      case (i: CIntExpr) :: ps => a |> acc(env, CIntExpr(i % m) :: ps, cont)
      case _ => error(s"Expected a C-Integer-Expression on the path.")
    }

    case ReorderAcc(n, _, idxF, a) => path match {
      case (i: CIntExpr) :: ps =>
        a |> acc(env, CIntExpr(idxF(i)) :: ps, cont)
      case _ => error(s"Expected a C-Integer-Expression on the path.")
    }

    case ScatterAcc(n, m, _, y, a) => path match {
      case (i: CIntExpr) :: ps =>
        val id = NatIdentifier(freshName("i"))
        Idx(n, IndexType(m), functional.NatAsIndex(n, Natural(i)), y) |>
          exp(env, Nil, yic => {
            C.AST.Block(immutable.Seq(
              C.AST.DeclStmt(C.AST.VarDecl(
                id.name, C.AST.Type.int, Some(yic)
              )),
              a |> acc(env, CIntExpr(id) :: ps, cont)
            ))
          })
      case _ => error(s"Expected a C-Integer-Expression on the path.")
    }

    case MapAcc(n, dt, _, f, a) => path match {
      case (i: CIntExpr) :: ps => f(IdxAcc(n, dt, functional.NatAsIndex(n, Natural(i)), a)) |> acc(env, ps, cont)
      case _ => error(s"Expected a C-Integer-Expression on the path.")
    }
    case MapFstAcc(_, dt2, dt3, f, a) => path match {
      case FstMember :: ps => f(PairAcc1(dt3, dt2, a)) |> acc(env, ps, cont)
      case SndMember :: ps => PairAcc2(dt3, dt2, a) |> acc(env, ps, cont)
      case _ => error(s"unexpected $path")
    }
    case MapSndAcc(dt1, _, dt3, f, a) => path match {
      case FstMember :: ps => PairAcc1(dt1, dt3, a) |> acc(env, ps, cont)
      case SndMember :: ps => f(PairAcc2(dt1, dt3, a)) |> acc(env, ps, cont)
      case _ => error(s"unexpected $path")
    }

    case IdxAcc(_, _, i, a) => CCodeGen.codeGenIdxAcc(i, a, env, path, cont)

    case DepIdxAcc(_, _, i, a) => a |> acc(env, CIntExpr(i) :: path, cont)

    case Proj1(pair) => Lifting.liftPair(pair)._1 |> acc(env, path, cont)
    case Proj2(pair) => Lifting.liftPair(pair)._2 |> acc(env, path, cont)

    case MkDPairSndAcc(_, _, a) => a |> acc(env, DPairSnd :: path, cont)

    case phrase@(Apply(_, _) | DepApply(_, _, _) |
                 Phrases.IfThenElse(_, _, _) | LetNat(_, _, _) | _: AccPrimitive) =>
      error(s"Don't know how to generate code for $phrase")
  }

  override def exp(env: Environment,
                   path: Path,
                   cont: Expr => Stmt): Phrase[ExpType] => Stmt = {
    case i@Identifier(_, ExpType(dt, _)) => generateAccess(dt,
      env.identEnv.applyOrElse(i, (_: Phrase[_]) => {
        throw new Exception(s"Expected to find `$i' in the environment: `${env.identEnv}'")
      }), path, env, cont)

    case Phrases.Literal(n) => path match {
      case Nil =>
        n.dataType match {
          case _: IndexType => cont(CCodeGen.codeGenLiteral(n))
          case _: ScalarType => cont(CCodeGen.codeGenLiteral(n))
          case _ => error("Expected an IndexType or ScalarType.")
        }
      case (i: CIntExpr) :: ps =>
        (n, n.dataType) match {
          case (ArrayData(elems), ArrayType(_, et)) => try {
            generateAccess(et, CCodeGen.codeGenLiteral(elems(i.eval)), ps, env, cont)
          } catch {
            case NotEvaluableException() => error(s"could not evaluate $i")
          }
          case _ => error("Expected an ArrayType.")
        }
      case _ => error(s"Unexpected: $n $path")
    }

    case Phrases.Natural(n) => cont(path match {
      case Nil => C.AST.ArithmeticExpr(n)
      case _ => error(s"Expected the path to be empty.")
    })

    case uop@UnaryOp(op, e) => uop.t.dataType match {
      case _: ScalarType => path match {
        case Nil => e |> exp(env, Nil, e =>
          cont(CCodeGen.codeGenUnaryOp(op, e)))
        case _ => error(s"Expected path to be empty")
      }
      case _ => error(s"Expected scalar types")
    }

    case bop@BinOp(op, e1, e2) => bop.t.dataType match {
      case _: ScalarType | NatType => path match {
        case Nil =>
          e1 |> exp(env, Nil, e1 =>
            e2 |> exp(env, Nil, e2 =>
              cont(CCodeGen.codeGenBinaryOp(op, e1, e2))))
        case _ => error(s"Expected path to be empty")
      }
      case _ => error(s"Expected scalar types, but ${bop.t.dataType} found")
    }

    case Cast(_, dt, e) => path match {
      case Nil =>
        e |> exp(env, Nil, e =>
          cont(C.AST.Cast(typ(dt), e)))
      case _ => error(s"Expected path to be empty")
    }

    case IndexAsNat(_, e) => e |> exp(env, path, cont)

    case NatAsIndex(_, e) => e |> exp(env, path, cont)

    case Split(n, _, _, _, e) => path match {
      case (i: CIntExpr) :: (j: CIntExpr) :: ps => e |> exp(env, CIntExpr(n * i + j) :: ps, cont)
      case _ => error(s"Expected two C-Integer-Expressions on the path.")
    }
    case Join(_, m, _, _, e) => path match {
      case (i: CIntExpr) :: ps => e |> exp(env, CIntExpr(i / m) :: CIntExpr(i % m) :: ps, cont)
      case _ => error(s"Expected two C-Integer-Expressions on the path.")
    }

    case part@Partition(_, _, _, _, e) => path match {
      case (i: CIntExpr) :: (j: CIntExpr) :: ps =>
        e |> exp(env, CIntExpr(BigSum(0, i - 1, x => part.lenF(x)) + j) :: ps, cont)
      case _ => error(s"Expected path to contain at least two elements")
    }

    case Zip(n, dt1, dt2, _, e1, e2) => path match {
      case (i: CIntExpr) :: (xj: PairAccess) :: ps => xj match {
        case FstMember => e1 |> exp(env, i :: ps, cont)
        case SndMember => e2 |> exp(env, i :: ps, cont)
      }
      case (i: CIntExpr) :: Nil =>
        val j = functional.NatAsIndex(n, Natural(i))
        MakePair(dt1, dt2, read, Idx(n, dt1, j, e1), Idx(n, dt2, j, e2)) |> exp(env, Nil, cont)
      case _ => error(s"unexpected $path")
    }

    case Unzip(_, _, _, _, e) => path match {
      case (xj: PairAccess) :: (i: CIntExpr) :: ps =>
        e |> exp(env, i :: xj :: ps, cont)
      case _ => error("Expected a tuple access followed by a C-Integer-Expression on the path.")
    }

    case DepZip(_, _, _, e1, e2) => path match {
      case (i: CIntExpr) :: (xj: PairAccess) :: ps => xj match {
        case FstMember => e1 |> exp(env, i :: ps, cont)
        case SndMember => e2 |> exp(env, i :: ps, cont)
      }
      case _ => error("Expected a C-Integer-Expression followed by a tuple access on the path.")
    }

    case r@MakePair(_, _, _, e1, e2) => path match {
      case (xj: PairAccess) :: ps => xj match {
        case FstMember => e1 |> exp(env, ps, cont)
        case SndMember => e2 |> exp(env, ps, cont)
      }
      case Nil =>
        e1 |> exp(env, Nil, ec1 =>
          e2 |> exp(env, Nil, ec2 => cont(C.AST.RecordLiteral(typ(r.t.dataType), ec1, ec2))))
      case _ => error(s"unexpected $path")
    }
    case Fst(_, _, e) => e |> exp(env, FstMember :: path, cont)
    case Snd(_, _, e) => e |> exp(env, SndMember :: path, cont)
    case DMatch(_, _, _, _, _, e) => e |> exp(env, path, cont)

    case Take(_, _, _, e) => e |> exp(env, path, cont)

    case Drop(n, _, _, e) => path match {
      case (i: CIntExpr) :: ps => e |> exp(env, CIntExpr(i + n) :: ps, cont)
      case _ => error(s"Expected a C-Integer-Expression on the path.")
    }

    case Cycle(_, m, _, e) => path match {
      case (i: CIntExpr) :: ps => e |> exp(env, CIntExpr(i % m) :: ps, cont)
      case _ => error(s"Expected a C-Integer-Expression on the path.")
    }

    case Reorder(n, _, _, idxF, _, a) => path match {
      case (i: CIntExpr) :: ps =>
        a |> exp(env, CIntExpr(idxF(i)) :: ps, cont)
      case _ => error(s"Expected a C-Integer-Expression on the path.")
    }

    case Gather(n, m, dt, y, e) => path match {
      case (i: CIntExpr) :: ps =>
        functional.Idx(n, dt, functional.Idx(m, IndexType(n), functional.NatAsIndex(m, Natural(i)), y), e) |>
          exp(env, ps, cont)
      case _ => error(s"unexpected $path")
    }

    case PadClamp(n, l, r, _, e) => path match {
      case (i: CIntExpr) :: ps =>
        e |> exp(env, CIntExpr(0) :: ps, left =>
          e |> exp(env, CIntExpr(n - 1) :: ps, right =>
            genPad(n, l, r, left, right, i, ps, e, env, cont)))
      case _ => error(s"Expected path to be not empty")
    }

    case PadCst(n, l, r, _, pad, array) => path match {
      case (i: CIntExpr) :: ps =>
        pad |> exp(env, ps, padExpr =>
          genPad(n, l, r, padExpr, padExpr, i, ps, array, env, cont))

      case _ => error(s"Expected path to be not empty")
    }

    case Slide(_, _, s2, _, e) => path match {
      case (i: CIntExpr) :: (j: CIntExpr) :: ps => e |> exp(env, CIntExpr(i * s2 + j) :: ps, cont)
      case _ => error(s"Expected two C-Integer-Expressions on the path.")
    }

    case Transpose(_, _, _, _, e) => path match {
      case (i: CIntExpr) :: (j: CIntExpr) :: ps => e |> exp(env, j :: i :: ps, cont)
      case _ => error(s"did not expect $path")
    }

    case TransposeDepArray(_, _, _, e) => path match {
      case (i: CIntExpr) :: (j: CIntExpr) :: ps => e |> exp(env, CIntExpr(j) :: CIntExpr(i) :: ps, cont)
      case _ => error(s"Expected two C-Integer-Expressions on the path.")
    }

    case Map(n, dt, _, _, f, e) => path match {
      case (i: CIntExpr) :: ps => f(Idx(n, dt, functional.NatAsIndex(n, Natural(i)), e)) |> exp(env, ps, cont)
      case _ => error(s"Expected a C-Integer-Expression on the path.")
    }

    // TODO: we could get rid of that
    case MapRead(n, dt1, dt2, f, e) => path match {
      case (i: CIntExpr) :: ps =>
        val continue_cmd =
          Identifier[ExpType ->: CommType](s"continue_$freshName", ExpType(dt2, read) ->: CommType())

        f(
          Idx(n, dt1, functional.NatAsIndex(n, Natural(i)), e)
        )(
          continue_cmd
        ) |> cmd(env updatedContEnv (continue_cmd -> (e => env => e |> exp(env, ps, cont))))
      case _ => error(s"Expected path to be not empty")
    }

    case GenerateCont(n, dt, f) => path match {
      case (i: CIntExpr) :: ps =>
        val continue_cmd =
          Identifier[ExpType ->: CommType](s"continue_$freshName", ExpType(dt, read) ->: CommType())

        f(functional.NatAsIndex(n, Natural(i)))(continue_cmd) |>
          cmd(env updatedContEnv (continue_cmd -> (e => env => e |> exp(env, ps, cont))))
      case _ => error(s"Expected path to be not empty")
    }

    case m@MakeArray(_) => path match {
      case (i: CIntExpr) :: ps => try {
        m.elements(i.eval) |> exp(env, ps, cont)
      } catch {
        case NotEvaluableException() => error(s"could not evaluate $i")
      }
      case _ => error(s"did not expect $path")
    }

    case Idx(_, _, i, e) => CCodeGen.codeGenIdx(i, e, env, path, cont)

    case DepIdx(_, _, i, e) => e |> exp(env, CIntExpr(i) :: path, cont)

    case ffc@ForeignFunctionCall(f, _) =>
      CCodeGen.codeGenForeignFunctionCall(f, ffc.inTs, ffc.outT, ffc.args, env, fe =>
        generateAccess(ffc.outT, fe, path, env, cont)
      )

    case Proj1(pair) => SimplifyNats.simplifyIndexAndNatExp(Lifting.liftPair(pair)._1) |> exp(env, path, cont)
    case Proj2(pair) => SimplifyNats.simplifyIndexAndNatExp(Lifting.liftPair(pair)._2) |> exp(env, path, cont)

    case phrase@(Apply(_, _) | DepApply(_, _, _) |
                 Phrases.IfThenElse(_, _, _) | LetNat(_, _, _) | _: ExpPrimitive) =>
      error(s"Don't know how to generate code for $phrase")
  }

  override def typ(dt: DataType): Type = {
    def typeToStructNameComponent(t:DataType):String = {
      t match {
        case IndexType(n) => s"idx$n"
        case ArrayType(n, t) => s"${n}_${typeToStructNameComponent(t)}"
        case PairType(a, b) => s"_${typeToStructNameComponent(a)}_${typeToStructNameComponent(b)}_"
        case _: ScalarType | _: FragmentType | _: IndexType | `NatType` | _: VectorType => typ(t).toString
        case _ => throw new Exception(s"Can't convert data type $t to struct name component")
      }
    }

    dt match {
      case b: rise.core.types.DataType.ScalarType => b match {
        case rise.core.types.DataType.bool => C.AST.Type.int
        case rise.core.types.DataType.int => C.AST.Type.int
        case rise.core.types.DataType.u8 => C.AST.Type.u8
        case rise.core.types.DataType.u16 => C.AST.Type.u16
        case rise.core.types.DataType.u32 => C.AST.Type.u32
        case rise.core.types.DataType.u64 => C.AST.Type.u64
        case rise.core.types.DataType.i8 => C.AST.Type.i8
        case rise.core.types.DataType.i16 => C.AST.Type.i16
        case rise.core.types.DataType.i32 => C.AST.Type.i32
        case rise.core.types.DataType.i64 => C.AST.Type.i64
        case rise.core.types.DataType.f16 => throw new Exception("f16 not supported")
        case rise.core.types.DataType.f32 => C.AST.Type.float
        case rise.core.types.DataType.f64 => C.AST.Type.double
      }
      case rise.core.types.DataType.NatType => C.AST.Type.int
      case _: rise.core.types.DataType.IndexType => C.AST.Type.int
      case _: rise.core.types.DataType.VectorType | _: rise.core.types.DataType.FragmentType =>
        throw new Exception(s"$dt types in C are not supported")
      case a: rise.core.types.DataType.ArrayType => C.AST.ArrayType(typ(a.elemType), Some(a.size))
      case a: rise.core.types.DataType.DepArrayType =>
        a.fdt match {
          case NatToDataLambda(_, body) =>
            C.AST.ArrayType(typ(body), Some(a.size)) // TODO: be more precise with the size?
          case _: NatToDataIdentifier =>  throw new Exception("This should not happen")
        }
      case r: rise.core.types.DataType.PairType =>
        C.AST.StructType("Record_" + typeToStructNameComponent(r.dt1) + "_" + typeToStructNameComponent(r.dt2),
          immutable.Seq(
            (typ(r.dt1), "_fst"),
            (typ(r.dt2), "_snd")))
      case rise.core.types.DataType.DepPairType(_, _, _) => C.AST.PointerType(C.AST.Type.u8)
      case _: rise.core.types.DataType.DataTypeIdentifier | _: rise.core.types.DataType.NatToDataApply |
           rise.core.types.DataType.ManagedBufferType(_) | rise.core.types.DataType.OpaqueType(_) =>
        throw new Exception(s"did not expect $dt")
    }
  }

  override def generateAccess(dt: DataType,
                              expr: Expr,
                              path: Path,
                              env: Environment,
                              cont: Expr => Stmt): Stmt = {
    path match {
      case Nil => cont(expr)
      case (xj: PairAccess) :: ps => dt match {
        case rt: PairType =>
          val (structMember, dt2) = xj match {
            case FstMember => ("_fst", rt.dt1)
            case SndMember => ("_snd", rt.dt2)
          }
          generateAccess(dt2, C.AST.StructMemberAccess(expr, C.AST.DeclRef(structMember)), ps, env, cont)
        case _ => throw new Exception("expected tuple type")
      }
      case (_: CIntExpr) :: _ =>
        dt match {
          case at: ArrayType =>
            val (dt2, k, ps) = CCodeGen.flattenArrayIndices(at, path)
            generateAccess(dt2, C.AST.ArraySubscript(expr, C.AST.ArithmeticExpr(k)), ps, env, cont)

          case dat: DepArrayType =>
            val (dt2, k, ps) = CCodeGen.flattenArrayIndices(dat, path)
            generateAccess(dt2, C.AST.ArraySubscript(expr, C.AST.ArithmeticExpr(k)), ps, env, cont)
          case x => throw new Exception(s"Expected an ArrayType that is accessed by the index but found $x instead.")
        }

      case DPairSnd :: ps =>
        dt match {
          case DepPairType(_, _, sndT) =>
            generateAccess(sndT,
              C.AST.Cast(C.AST.PointerType(C.AST.Type.getBaseType(typ(sndT))),
                C.AST.BinaryExpr(expr, C.AST.BinaryOperator.+, C.AST.Literal("sizeof(uint32_t)"))
            ), ps, env, cont)

          case other => throw new Exception(s"Expected a Dependent Pair but $other found instead")
        }
      case _ =>
        throw new Exception(s"Can't generate access for `$dt' with `${path.mkString("[", "::", "]")}'")
    }
  }

  private def generateLetNat[T <: PhraseType](binder:LetNatIdentifier,
                             defn:Phrase[T],
                             env:Environment,
                             cont:(CodeGenerator, Environment) => Stmt):Stmt = {
    cont(this, env updatedNatEnv((binder, defn.asInstanceOf[Phrase[PhraseType]])))
  }

  /* Take a phrase representing a function (may have multiple level of lambas/DepLambda[NatKind, _]),
     and a series of arguments, and generates the code of the function applied to the arguments.
     Since the generated code is inlined directly, no special measures need to be taken to handle variables captured
     in the phrase
   */
  def generateInlinedCall[T <: PhraseType](phrase: Phrase[T],
                                           env:Environment,
                                           args:Iterable[Either[Phrase[ExpType], Nat]],
                                           cont:Expr => Stmt):Stmt = {

    def error(s:String) = throw new Exception(s + " in deferred function generation")
    phrase match {
      case l: Lambda[ExpType, _]@unchecked => args.headOption match {
        case Some(Right(_)) => error("Nat argument passed but phrase type arg expected")
        case None => error("Parameter missing")
        case Some(Left(param)) => generateInlinedCall(l(param), env, args.tail, cont)
      }
      case ndl: DepLambda[Nat, NatIdentifier, _]@unchecked => args.headOption match {
        case Some(Right(nat)) => generateInlinedCall(ndl(nat), env, args.tail, cont)
        case None => error("Parameter missing")
        case Some(Left(_)) => error("Expression phrase argument passed but nat expected")
      }
      case ep: Phrase[ExpType]@unchecked => args.headOption match {
        case Some(_) => error("Too many arguments in deferred funct")
        case None => ep |> exp(env, List(), cont)
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
  protected def generateNatDependentBody(`for`: NatIdentifier,
                                         phrase: Phrase[CommType],
                                         at: ArithExpr,
                                         env: Environment): Block = {
    shine.DPIA.Types.substitute(at, `for`, in = phrase) |> (p => {
      val newIdentEnv = env.identEnv.map {
        case (Identifier(name, AccType(dt)), declRef) =>
          (Identifier(name, AccType(substituteNatInType(at, `for`, in = dt))), declRef)
        case (Identifier(name, ExpType(dt, a)), declRef) =>
          (Identifier(name, ExpType(substituteNatInType(at, `for`, in = dt), a)), declRef)
        case x => x
      }
      C.AST.Block(immutable.Seq(p |> this.cmd(env.copy(identEnv = newIdentEnv))))
    })
  }

  protected object CCodeGen {
    def codeGenNew(dt: DataType,
                   v: Identifier[VarType],
                   p: Phrase[CommType],
                   env: Environment): Stmt = {
      val ve = Identifier(s"${v.name}_e", v.t.t1)
      val va = Identifier(s"${v.name}_a", v.t.t2)
      val vC = C.AST.DeclRef(v.name)

      C.AST.Block(immutable.Seq(
        C.AST.DeclStmt(C.AST.VarDecl(vC.name, typ(dt))),
        Phrase.substitute(PhrasePair(ve, va), `for` = v, `in` = p) |> cmd(env updatedIdentEnv (ve -> vC)
            updatedIdentEnv (va -> vC))))
    }

    def codeGenNewDoubleBuffer(dt: ArrayType,
                               in: Phrase[ExpType],
                               out: Phrase[AccType],
                               ps: Identifier[VarType x CommType x CommType],
                               p: Phrase[CommType],
                               env: Environment): Stmt = {
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

      Block(immutable.Seq(
        // create variables: `tmp1', `tmp2`, `in_ptr', and `out_ptr'
        DeclStmt(VarDecl(tmp1.name, typ(dt))),
        DeclStmt(VarDecl(tmp2.name, typ(dt))),
        in |> exp(env, CIntExpr(0) :: Nil, e => makePointerDecl(in_ptr.name, dt.elemType, UnaryExpr(&, e))),
        makePointerDecl(out_ptr.name, dt.elemType, tmp1),
        // create boolean flag used for swapping
        DeclStmt(VarDecl(flag.name, Type.uchar, Some(Literal("1")))),
        // generate body
        Phrase.substitute(PhrasePair(PhrasePair(PhrasePair(ve, va), swap), done), `for` = ps, `in` = p) |>
          cmd(env updatedIdentEnv (ve -> in_ptr) updatedIdentEnv (va -> out_ptr)
            updatedCommEnv (swap -> {
            Block(immutable.Seq(
              ExprStmt(Assignment(in_ptr, TernaryExpr(flag, tmp1, tmp2))),
              ExprStmt(Assignment(out_ptr, TernaryExpr(flag, tmp2, tmp1))),
              // toggle flag with xor
              ExprStmt(Assignment(flag, BinaryExpr(flag, ^, Literal("1"))))))
          })
            updatedCommEnv (done -> {
            Block(immutable.Seq(
              ExprStmt(Assignment(in_ptr, TernaryExpr(flag, tmp1, tmp2))),
              out |> acc(env, CIntExpr(0) :: Nil, o => ExprStmt(Assignment(out_ptr, UnaryExpr(&, o))))))
          }))
      ))
    }

    def codeGenFor(n: Nat,
                   i: Identifier[ExpType],
                   p: Phrase[CommType],
                   unroll:Boolean,
                   env: Environment): Stmt = {
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
          C.AST.Stmts(C.AST.Stmts(
            C.AST.Comment("iteration count is exactly 1, no loop emitted"),
            C.AST.DeclStmt(C.AST.VarDecl(cI.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0))))),
            p |> updatedGen.cmd(env updatedIdentEnv (i -> cI)))

        case _ =>
          val init = C.AST.VarDecl(cI.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0)))
          val cond = C.AST.BinaryExpr(cI, C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n))
          val increment = C.AST.Assignment(cI, C.AST.ArithmeticExpr(NamedVar(cI.name, range) + 1))

          C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
            C.AST.Block(immutable.Seq(p |> updatedGen.cmd(env updatedIdentEnv (i -> cI)))))
      }})
    }

    def codeGenForNat(n: Nat,
                      i: NatIdentifier,
                      p: Phrase[CommType],
                      unroll:Boolean,
                      env: Environment): Stmt = {
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
          C.AST.Stmts(C.AST.Stmts(
            C.AST.Comment("iteration count is exactly 1, no loop emitted"),
            C.AST.DeclStmt(C.AST.VarDecl(cI.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0))))),
            p |> updatedGen.cmd(env))

        case _ =>
          val init = C.AST.VarDecl(cI.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0)))
          val cond = C.AST.BinaryExpr(cI, C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n))
          val increment = C.AST.Assignment(cI, C.AST.ArithmeticExpr(NamedVar(cI.name, range) + 1))

          C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
            updatedGen.generateNatDependentBody(`for` = i, `phrase` = p, at = NamedVar(cI.name, range), env)
          )
      }})
    }

    def codeGenIdxAcc(i: Phrase[ExpType],
                      a: Phrase[AccType],
                      env: Environment,
                      ps: Path,
                      cont: Expr => Stmt): Stmt = {
      i |> exp(env, Nil, {
        case C.AST.Literal(text) => a |> acc(env, CIntExpr(Cst(text.toInt)) :: ps, cont)
        case C.AST.DeclRef(name) => a |> acc(env, CIntExpr(NamedVar(name, ranges(name))) :: ps, cont)
        case C.AST.ArithmeticExpr(ae) => a |> acc(env, CIntExpr(ae) :: ps, cont)
        case cExpr:C.AST.Expr =>
          val arithVar = NamedVar(freshName("idxAcc"))
          C.AST.Block(immutable.Seq(
            C.AST.DeclStmt(C.AST.VarDecl(arithVar.name, C.AST.Type.int, Some(cExpr))),
            a |> acc(env, CIntExpr(arithVar) :: ps, cont)
          ))
      })
    }

    def codeGenLiteral(d: Data): Expr = {
      d match {
        case NatData(n)       => C.AST.ArithmeticExpr(n)
        case IndexData(i, _)  => C.AST.ArithmeticExpr(i)
        case _: IntData | _: FloatData | _: DoubleData | _: BoolData =>
          C.AST.Literal(d.toString)
        case NatAsIntData(n) =>
          C.AST.Literal(n.toString)
        case ArrayData(a) => d.dataType match {
          case ArrayType(_, ArrayType(_, _)) =>
            codeGenLiteral(ArrayData(a.flatten(d => d.asInstanceOf[ArrayData].a)))
          case _ => C.AST.ArrayLiteral(typ(d.dataType).asInstanceOf[C.AST.ArrayType], a.map(codeGenLiteral))
        }
        case PairData(fst, snd) =>
          C.AST.RecordLiteral(typ(d.dataType), codeGenLiteral(fst), codeGenLiteral(snd))
        case VectorData(_) => throw new Exception("VectorData not supported in C")
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
      i |> exp(env, Nil, {
        case C.AST.DeclRef(name) => e |> exp(env, CIntExpr(NamedVar(name, ranges(name))) :: ps, cont)
        case C.AST.ArithmeticExpr(ae) => e |> exp(env, CIntExpr(ae) :: ps, cont)
        case cExpr:C.AST.Expr =>
          val arithVar = NamedVar(freshName("idx"))
          C.AST.Block(immutable.Seq(
            C.AST.DeclStmt(C.AST.VarDecl(arithVar.name, C.AST.Type.int, Some(cExpr))),
            e |> exp(env, CIntExpr(arithVar) :: ps, cont)
          ))
      })
    }

    def codeGenForeignFunctionCall(funDecl: rise.core.ForeignFunction.Decl,
                                   inTs: collection.Seq[DataType],
                                   outT: DataType,
                                   args: collection.Seq[Phrase[ExpType]],
                                   env: Environment,
                                   cont: Expr => Stmt): Stmt =
    {
      funDecl.definition match {
        case Some(funDef) =>
          addDeclaration(
            C.AST.FunDecl(funDecl.name,
              returnType = typ(outT),
              params = (funDef.params zip inTs).map {
                case (name, dt) => C.AST.ParamDecl(name, typ(dt))
              },
              body = C.AST.Code(funDef.body)))
        case _ =>
      }

      codeGenForeignCall(funDecl.name, args, env, Nil, cont)
    }

    def codeGenForeignCall(name: String,
                           args: collection.Seq[Phrase[ExpType]],
                           env: Environment,
                           args_ps: Path,
                           cont: Expr => Stmt): Stmt =
    {
      def iter(args: collection.Seq[Phrase[ExpType]], res: VectorBuilder[Expr]): Stmt = {
        //noinspection VariablePatternShadow
        args match {
          case a +: args =>
            a |> exp(env, args_ps, a => iter(args, res += a))
          case _ => cont(
            C.AST.FunCall(C.AST.DeclRef(name), res.result()))
        }
      }

      iter(args, new VectorBuilder())
    }

    def codeGenBinaryOp(op: Operators.Binary.Value,
                        e1: Expr,
                        e2: Expr): Expr = {
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

    def flattenIndices(dataType: DataType, indicies:List[Nat]): Nat = {
      (dataType, indicies) match {
        case (array:ArrayType, index::rest) =>
          numberOfElementsUntil(array, index) + flattenIndices(array.elemType, rest)
        case (array:DepArrayType, index::rest) =>
          array.fdt match {
            case NatToDataLambda(_, body) =>
              numberOfElementsUntil(array, index) + flattenIndices(body, rest)
            case _: NatToDataIdentifier => throw new Exception(s"This should not happen")
          }
        case (_,  Nil) => 0
        case t => throw new Exception(s"This should not happen, pair $t")
      }
    }

    //Computes the total number of element in an array at a given offset
    def numberOfElementsUntil(dt:ArrayType, at:Nat): Nat = {
      types.DataTypeOps.getTotalNumberOfElements(dt.elemType)*at
    }

    def numberOfElementsUntil(dt:DepArrayType, at:Nat): Nat = {
      dt.fdt match {
        case NatToDataLambda(x, body) =>
          BigSum(from=0, upTo = at-1, `for`=x, types.DataTypeOps.getTotalNumberOfElements(body))
        case _: NatToDataIdentifier => throw new Exception(s"This should not happen")
      }
    }

    private def getIndexVariablesScopes(dt:DataType):List[Option[NatIdentifier]] = {
      dt match {
        case ArrayType(_ , et) => None::getIndexVariablesScopes(et)
        case DepArrayType(_, NatToDataLambda(i, et)) => Some(i)::getIndexVariablesScopes(et)
        case _ => Nil
      }
    }

    private def buildSubMap(dt: DataType,
                            indices: immutable.Seq[Nat]): Predef.Map[Nat, Nat]  = {
      val bindings = getIndexVariablesScopes(dt)
      bindings.zip(indices).map({
        case (Some(binder), index) => Some((binder, index))
        case _ => None
      }).filter(_.isDefined).map(_.get).toMap[Nat, Nat]
    }

    implicit def convertBinaryOp(op: Operators.Binary.Value): shine.C.AST.BinaryOperator.Value = {
      import Operators.Binary._
      op match {
        case ADD => C.AST.BinaryOperator.+
        case SUB => C.AST.BinaryOperator.-
        case MUL => C.AST.BinaryOperator.*
        case DIV => C.AST.BinaryOperator./
        case MOD => C.AST.BinaryOperator.%
        case GT => C.AST.BinaryOperator.>
        case LT => C.AST.BinaryOperator.<
        case EQ => C.AST.BinaryOperator.==
      }
    }

    implicit def convertUnaryOp(op: Operators.Unary.Value): shine.C.AST.UnaryOperator.Value = {
      import Operators.Unary._
      op match {
        case NEG => C.AST.UnaryOperator.-
        case NOT => C.AST.UnaryOperator.!
      }
    }

    def makePointerDecl(name: String,
                        elemType: DataType,
                        expr: Expr): Stmt = {
      import C.AST._
      DeclStmt(
        VarDecl(name, PointerType(typ(elemType)), Some(expr)))
    }
  }

  protected def applySubstitutions(n: Nat,
                                   identEnv: immutable.Map[Identifier[_ <: BasePhraseType], C.AST.DeclRef]): Nat = {
    // lift the substitutions from the Phrase level to the ArithExpr level
    val substitionMap = identEnv.filter(_._1.t match {
      case ExpType(IndexType(_), _) => true
      case AccType(IndexType(_)) => true
      case _ => false
    }).map(i => (NamedVar(i._1.name), NamedVar(i._2.name))).toMap[ArithExpr, ArithExpr]
    ArithExpr.substitute(n, substitionMap)
  }


  private def visitAndGenerateNat(node:Stmt, env:Environment):Stmt = {
    C.AST.Nodes.VisitAndGenerateStmt(node, new C.AST.Nodes.VisitAndGenerateStmt.Visitor() {
      override def onExpr(e: Expr, cont: Expr => Stmt): Stmt = e match {
        case C.AST.ArithmeticExpr(ae) => genNat(ae, env, cont)
        case other => cont(other)
      }
    })
  }

  override def genNat(n:Nat, env:Environment, cont:Expr => Stmt):Stmt = {
    def boolExp(b:BoolExpr, env:Environment, cont:Expr => Stmt):Stmt= b match {
      case BoolExpr.True => cont(C.AST.Literal("true"))
      case BoolExpr.False => cont(C.AST.Literal("false"))
      case BoolExpr.ArithPredicate(lhs, rhs, op) =>
        val cOp = op match {
          case ArithPredicate.Operator.!= => C.AST.BinaryOperator.!=
          case ArithPredicate.Operator.== => C.AST.BinaryOperator.==
          case ArithPredicate.Operator.< => C.AST.BinaryOperator.<
          case ArithPredicate.Operator.<= => C.AST.BinaryOperator.<=
          case ArithPredicate.Operator.> => C.AST.BinaryOperator.>
          case ArithPredicate.Operator.>= => C.AST.BinaryOperator.>=
        }
        genNat(lhs, env, lhs => genNat(rhs, env, rhs => cont( C.AST.BinaryExpr(lhs, cOp, rhs))))
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
    def genBinopFold(nats: Iterable[Nat],
                     op: C.AST.BinaryOperator.Value,
                     default: Expr,
                     cont: Expr => Stmt,
                     accum: Option[Expr] = None): Stmt = {
      nats.headOption match {
        case None => cont(accum.getOrElse(default))
        case Some(nat) =>
          accum match {
            case None => genNat(nat, env, exp => genBinopFold(nats.tail, op, default, cont, Some(exp)))
            case Some(acc) =>
              genNat(nat, env, exp => genBinopFold(nats.tail, op, default, cont, Some(C.AST.BinaryExpr(acc, op, exp))))
          }
      }
    }
       n match {
         case Cst(c) => cont(AST.Literal(c.toString))
         case Pow(b, ex) =>
           ex match {
             case Cst(-1) =>
               if (Cst(1) % b != Cst(0)) {
                 println(s"WARNING: 1 /^ $b might have a fractional part")
               }
               genNat(b, env, b =>
                 cont(C.AST.BinaryExpr(AST.Literal("1"), C.AST.BinaryOperator./, b)))
             case Cst(2) =>
               genNat(b, env, b => cont(AST.BinaryExpr(b,AST.BinaryOperator.*, b)))
             case _ =>
               // FIXME: this often generates functionally incorrect code
               genNat(b, env, b =>
                 genNat(ex, env, ex =>
                   cont(AST.Cast(AST.Type.int, AST.FunCall(AST.DeclRef("pow"), immutable.Seq(
                     AST.Cast(AST.Type.float, b), AST.Cast(AST.Type.float, ex))))
                   )
                 )
               )
           }
         case Log(b, x) =>
           genNat(b, env, b =>
             genNat(x, env, x =>
               cont(AST.Cast(AST.Type.int, AST.FunCall(AST.DeclRef("log" + b), immutable.Seq(
                 AST.Cast(AST.Type.float, x))
               )))
             )
           )

         case Prod(es) =>
           var (num, denum) = es.partition({
             case Pow(_, Cst(-1)) => false
             case _ => true
           })
           denum = denum.map {
             case Pow(b, Cst(-1)) => b
             case _ => ???
            }
           if (denum.nonEmpty) { // num /^ denum
             val aNum = num.fold(1: ArithExpr)(_*_)
             val aDenum = denum.fold(1: ArithExpr)(_*_)
             if (aNum % aDenum != Cst(0)) {
               println(s"WARNING: $aNum /^ $aDenum might have a fractional part")
             }
             genBinopFold(num, AST.BinaryOperator.*, AST.Literal("0"), lhs =>
               genBinopFold(denum, AST.BinaryOperator.*, AST.Literal("0"), rhs =>
                 cont(C.AST.BinaryExpr(lhs, C.AST.BinaryOperator./, rhs))))
           } else {
             genBinopFold(es, AST.BinaryOperator.*, AST.Literal("0"), cont)
           }

         case Sum(es) => genBinopFold(es, AST.BinaryOperator.+, AST.Literal("0"), cont)

         case Mod(a, n) =>
           if (arithexpr.arithmetic.ArithExpr.mightBeNegative(a)) {
             println(s"WARNING: $a % $n might operate on negative values")
           }
           genNat(a, env, a => genNat(n, env, n => cont(AST.BinaryExpr(a, AST.BinaryOperator.%, n))))

         case v:Var => cont(C.AST.DeclRef(v.toString))

         case IntDiv(n, d) =>
           genNat(n, env, n => genNat(d, env, d => cont(AST.BinaryExpr(n, AST.BinaryOperator./, d))))

         case LShift(a, b) =>
           genNat(a, env, a => genNat(b, env, b => cont(AST.BinaryExpr(a, AST.BinaryOperator.<<, b))))

         case lu:Lookup =>
           cont(AST.FunCall(AST.DeclRef(s"lookup${lu.id}"), immutable.Seq(AST.Literal(lu.index.toString))))

         case arithexpr.arithmetic.IfThenElse(cond, trueBranch, falseBranch) =>
           boolExp(cond, env,
             cond => genNat(trueBranch, env,
               trueBranch => genNat(falseBranch, env,
                 falseBranch => cont(AST.TernaryExpr(cond, trueBranch, falseBranch)))))


         case natFunCall: NatFunCall =>
           val phrase = env.letNatEnv(natFunCall.fun)

           val args = natFunCall.args.map({
             case NatArg(argN) => Right(argN)
             case LetNatIdArg(ident) =>
               val argPhrase = env.letNatEnv(ident)
               if(!argPhrase.t.isInstanceOf[ExpType]) {
                 throw new Exception("Cannot use non-expression let nat arguments in natFunCall")
               }
               Left(argPhrase.asInstanceOf[Phrase[ExpType]])
           })

           visitAndGenerateNat(generateInlinedCall(phrase, env, args, cont), env)

         case sp: SteppedCase => genNat(sp.intoIfChain(), env, cont)

         case BigSum(variable, body) =>
             println(s"Generating for loop for big sum $n")
             genNat(variable.from, env, from => {
               genNat(variable.upTo, env, upTo => {
                 genNat(body, env, bodyE => {
                   val loopVar = C.AST.DeclRef(variable.toString)

                   val init = C.AST.DeclStmt(C.AST.VarDecl(loopVar.name, C.AST.Type.int, init = Some(from)))
                   val cond = C.AST.BinaryExpr(loopVar, C.AST.BinaryOperator.<, upTo)
                   val increment =
                     C.AST.Assignment(loopVar, C.AST.ArithmeticExpr(NamedVar(loopVar.name, variable.range) + 1))

                   val accumVar = C.AST.VarDecl(freshName("accum_"), C.AST.Type.int, Some(AST.Literal("0")))


                   val forLoop = C.AST.ForLoop(init,
                     cond,
                     increment,
                     Block(
                       immutable.Seq(
                         C.AST.ExprStmt(C.AST.Assignment(
                           C.AST.DeclRef(accumVar.name),
                           C.AST.BinaryExpr(C.AST.DeclRef(accumVar.name), C.AST.BinaryOperator.+, bodyE)
                         ))
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
                 })
               })
             })
         case otherwise => throw new Exception(s"Don't know how to print $otherwise")
       }
  }

  protected def genPad(n: Nat, l: Nat, r: Nat,
                       left: Expr, right: Expr,
                       i: CIntExpr, ps: Path,
                       array: Phrase[ExpType],
                       env: Environment,
                       cont: Expr => Stmt): Stmt = {
    // FIXME: we should know that (i - l) is in [0; n[ here
    array |> exp(env, CIntExpr(i - l) :: ps, arrayExpr => {

      def cOperator(op:ArithPredicate.Operator.Value):C.AST.BinaryOperator.Value = op match {
        case ArithPredicate.Operator.< => C.AST.BinaryOperator.<
        case ArithPredicate.Operator.> => C.AST.BinaryOperator.>
        case ArithPredicate.Operator.>= => C.AST.BinaryOperator.>=
        case _ => null
      }

      def genBranch(lhs:ArithExpr, rhs:ArithExpr,
                    operator:ArithPredicate.Operator.Value, taken:Expr, notTaken:Expr): Expr = {
        import BoolExpr._
        arithPredicate(lhs, rhs, operator) match {
          case True => taken
          case False => notTaken
          case _ => C.AST.TernaryExpr(
            C.AST.BinaryExpr(C.AST.ArithmeticExpr(lhs), cOperator(operator), C.AST.ArithmeticExpr(rhs)),
            taken, notTaken)
        }
      }
      cont(
        genBranch(i, l, ArithPredicate.Operator.<, left,
          genBranch(i, l + n, ArithPredicate.Operator.<, arrayExpr, right)))
    })
  }
}

