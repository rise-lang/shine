package shine.OpenMP

import arithexpr.arithmetic
import arithexpr.arithmetic._
import rise.core.types.{DataType, NatKind}
import rise.core.types.DataType._
import rise.core.substitute.{natInType => substituteNatInType}
import shine.C.AST.{ArraySubscript, Decl}
import shine.C.Compilation.CodeGenerator.CIntExpr
import shine.C.Compilation.{CodeGenerator => CCodeGenerator}
import shine.DPIA.DSL._
import shine.DPIA.primitives.imperative._
import shine.DPIA.Phrases._
import shine.DPIA.Types.{AccType, CommType, ExpType, PhraseType}
import shine.DPIA.primitives.functional._
import shine.DPIA.{ArrayData, Compilation, Data, Nat, NatIdentifier, Phrases, VectorData, error, freshName}
import shine.OpenMP.primitives.imperative.{ParFor, ParForNat}
import shine.{C, _}

import scala.collection.{immutable, mutable}

object CodeGenerator {
  def apply(): CodeGenerator =
    new CodeGenerator(mutable.ListBuffer[Decl](), immutable.Map[String, arithmetic.Range]())
}

class CodeGenerator(override val decls: CCodeGenerator.Declarations,
                    override val ranges: CCodeGenerator.Ranges)
  extends C.Compilation.CodeGenerator(decls, ranges)
{
  override def name: String = "OpenMP"

  override def translationContext: Compilation.TranslationContext = new TranslationContext()

  override def updatedRanges(key: String, value: arithexpr.arithmetic.Range): CodeGenerator =
    new CodeGenerator(decls, ranges.updated(key, value))


  override def cmd(env: Environment): Phrase[CommType] => Stmt = {
    case ParFor(n, dt, a, Lambda(i, Lambda(o, p))) =>
      OpenMPCodeGen.codeGenParFor(n, dt, a, i, o, p, env)
    case ForVec(n, dt, a, Lambda(i, Lambda(o, p))) =>
      OpenMPCodeGen.codeGenParForVec(n, dt, a, i, o, p, env)
    case ParForNat(n, _, a, DepLambda(NatKind, i, Lambda(o, p))) =>
      OpenMPCodeGen.codeGenParForNat(n, a, i, o, p, env)
    case phrase => phrase |> super.cmd(env)
  }

  override def acc(env: Environment,
                   path: Path,
                   cont: Expr => Stmt): Phrase[AccType] => Stmt = {
    case AsVectorAcc(_, m, _, a) => path match {
      case (i: CIntExpr) :: ps => a |> acc(env, CIntExpr(i / m) :: ps, cont)
      case _ => error(s"Expected path to be not empty")
    }
    case AsScalarAcc(n, _, dt, a) => path match {
      case (i: CIntExpr) :: (j: CIntExpr) :: ps =>
        a |> acc(env, CIntExpr((i * n) + j) :: ps, cont)

      case (i: CIntExpr) :: Nil =>
        a |> acc(env, CIntExpr(i * n) :: Nil, {
          case ArraySubscript(v, idx) =>
            // emit something like: ((struct float4 *)v)[idx]
            val ptrType = C.AST.PointerType(typ(VectorType(n, dt)))
            cont(C.AST.ArraySubscript(C.AST.Cast(ptrType, v), idx))
        })
      case _ => error(s"Expected path to be not empty")
    }
    case IdxVecAcc(_, _, i, a) => CCodeGen.codeGenIdxAcc(i, a, env, path, cont)
    case phrase => phrase |> super.acc(env, path, cont)
  }

  override def exp(env: Environment,
                   path: Path,
                   cont: Expr => Stmt): Phrase[ExpType] => Stmt = {
    case phrase@Phrases.Literal(n) => (path, n.dataType) match {
      case (Nil, _: VectorType) => cont(OpenMPCodeGen.codeGenLiteral(n))
      case ((i: CIntExpr) :: Nil, _: VectorType) =>
        cont(C.AST.ArraySubscript(OpenMPCodeGen.codeGenLiteral(n), C.AST.ArithmeticExpr(i)))
      case _ => phrase |> super.exp(env, path, cont)
    }
    case phrase@UnaryOp(op, e) => phrase.t.dataType match {
      case _: VectorType => path match {
        case i :: ps => e |> exp(env, i :: ps, e => cont(CCodeGen.codeGenUnaryOp(op, e)))
        case _ => error(s"Expected path to be not empty")
      }
      case _ => phrase |> super.exp(env, path, cont)
    }
    case phrase@BinOp(op, e1, e2) => phrase.t.dataType match {
      case _: VectorType => path match {
        case i :: ps =>
          e1 |> exp(env, i :: ps, e1 =>
            e2 |> exp(env, i :: ps, e2 =>
              cont(CCodeGen.codeGenBinaryOp(op, e1, e2))))
        case _ => error(s"Expected path to be not empty")
      }
      case _ => phrase |> super.exp(env, path, cont)
    }
    case ffc@ForeignFunctionCall(f, _) =>
      OpenMPCodeGen.codeGenForeignFunctionCall(f, ffc.inTs, ffc.outT, ffc.args, env, path, cont)
    case AsVectorAligned(n, _, dt, _, e) => path match {
      case (i: CIntExpr) :: (j: CIntExpr) :: ps =>
        e |> exp(env, CIntExpr((i * n) + j) :: ps, cont)

      case (i: CIntExpr) :: Nil =>
        e |> exp(env, CIntExpr(i * n) :: Nil, {
          case ArraySubscript(v, idx) =>
            // emit something like: ((struct float4 *)v)[idx]
            val ptrType = C.AST.PointerType(typ(VectorType(n, dt)))
            cont(C.AST.ArraySubscript(C.AST.Cast(ptrType, v), idx))
        })
      case _ => error(s"Expected path to be not empty")
    }
    case AsScalar(n, _, _, _, e) => path match {
      case (i: CIntExpr) :: ps => e |> exp(env, CIntExpr(i / n) :: CIntExpr(i % n) :: ps, cont)
      case _ => error(s"Expected path to be not empty")
    }
    // TODO: this has to be refactored
    case VectorFromScalar(n, st, e) => path match {
      case (_: CIntExpr) :: ps =>
        // in this case we index straight into the vector build from a single scalar
        // it is equivalent to return the scalar `e' without boxing and unboxing it
        e |> exp(env, ps, cont)
      //          C.AST.ArraySubscript(
      //            C.AST.Literal( "(" + s"($st[$n]){" + C.AST.Printer(exp(e, env, ps)) + "})" ),
      //            C.AST.ArithmeticExpr(i))

      case Nil =>
        e |> exp(env, Nil, e =>
          cont(C.AST.Literal("(" + s"($st[$n]){" + C.AST.Printer(e) + "})")))

      case _ => error(s"Didn't expect path: $path")
    }
    case IdxVec(_, _, i, e) => CCodeGen.codeGenIdx(i, e, env, path, cont)
    case phrase => phrase |> super.exp(env, path, cont)
  }

  override def typ(dt: DataType): Type = {
    dt match {
      case v: VectorType =>
        // this sets the representation of vector types in C:
        // struct float4 {
        //    float data[4];
        // };
        C.AST.StructType(s"${v.elemType}${v.size}",
          immutable.Seq((C.AST.ArrayType(typ(v.elemType), Some(v.size)), "data")))
      case _ => super.typ(dt)
    }
  }

  override def generateAccess(dt: DataType,
                              expr: Expr,
                              path: Path,
                              env: Environment,
                              cont: Expr => Stmt): Stmt = {
    (path, dt) match {
      case ((i: CIntExpr) :: _, _: VectorType) =>
        val data = C.AST.StructMemberAccess(expr, C.AST.DeclRef("data"))
        cont(C.AST.ArraySubscript(data, C.AST.ArithmeticExpr(i)))
      case _ => super.generateAccess(dt, expr, path, env, cont)
    }
  }

  protected object OpenMPCodeGen {
    def codeGenParFor(n: Nat,
                      dt: DataType,
                      a: Phrase[AccType],
                      i: Identifier[ExpType],
                      o: Phrase[AccType],
                      p: Phrase[CommType],
                      env: Environment): Stmt = {
      val cI = C.AST.DeclRef(freshName("i_"))
      val range = RangeAdd(0, n, 1)
      val updatedGen = updatedRanges(cI.name, range)

      applySubstitutions(n, env.identEnv) |> (n => {

      val init = C.AST.VarDecl(cI.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0)))
      val cond = C.AST.BinaryExpr(cI, C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n))
      val increment = shine.C.AST.Assignment(cI, C.AST.ArithmeticExpr(NamedVar(cI.name, range) + 1))

      Phrase.substitute(a `@` i, `for` = o, `in` = p) |> (p =>

      env.updatedIdentEnv(i -> cI) |> (env =>

      range.numVals match {
        // iteration count is 0 => skip body; no code to be emitted
        case Cst(0) => C.AST.Comment("iteration count is 0, no loop emitted")
        // iteration count is 1 => no loop
        case Cst(1) =>
          C.AST.Stmts(C.AST.Stmts(
            C.AST.Comment("iteration count is exactly 1, no loop emitted"),
            C.AST.DeclStmt(C.AST.VarDecl(cI.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0))))),
            p |> updatedGen.cmd(env))
        // default case
        case _ =>C.AST.Stmts(
        C.AST.Code("#pragma omp parallel for"),
        C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
          C.AST.Block(immutable.Seq(p |> updatedGen.cmd(env)))))
      }))})
    }

    def codeGenParForNat(n: Nat,

                         a: Phrase[AccType],
                         i: NatIdentifier,
                         o: Phrase[AccType],
                         p: Phrase[CommType],
                         env: Environment): Stmt = {

      val cI = C.AST.DeclRef(freshName("i_"))
      val range = RangeAdd(0, n, 1)
      val updatedGen = updatedRanges(cI.name, range)

      applySubstitutions(n, env.identEnv) |> (n => {

      val init = C.AST.VarDecl(cI.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0)))
      val cond = C.AST.BinaryExpr(cI, C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n))
      val increment = shine.C.AST.Assignment(cI, C.AST.ArithmeticExpr(NamedVar(cI.name, range) + 1))

      //FIRST we must substitute in the indexing of o in the phrase
       Phrase.substitute(a `@d` i, `for` = o, `in` = p) |> (p =>
      //THEN and only THEN we can change the type to use the new index var
         shine.DPIA.Types.substitute(NamedVar(cI.name, range), `for` = i, in = p) |> (p =>

      env.copy(identEnv = env.identEnv.map {
        case (Identifier(name, AccType(dt)), declRef) =>
          (Identifier(name, AccType(substituteNatInType(NamedVar(cI.name, range), `for` = i, in = dt))), declRef)
        case (Identifier(name, ExpType(dt, w)), declRef) =>
          (Identifier(name, ExpType(substituteNatInType(NamedVar(cI.name, range), `for` = i, in = dt), w)), declRef)
        case x => x
      }) |> (env =>

      range.numVals match {
        // iteration count is 0 => skip body; no code to be emitted
        case Cst(0) => C.AST.Comment("iteration count is 0, no loop emitted")
        // iteration count is 1 => no loop
//        case Cst(1) =>
//          C.AST.Stmts(C.AST.Stmts(
//            C.AST.Comment("iteration count is exactly 1, no loop emitted"),
//            C.AST.DeclStmt(C.AST.VarDecl(cI.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0))))),
//            updatedGen.cmd(p, env))
        // default case
        case _ =>C.AST.Stmts(
        C.AST.Code("#pragma omp parallel for"),
        C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
          C.AST.Block(immutable.Seq(p |> updatedGen.cmd(env)))))
  })))})
    }

    def codeGenParForVec(n: Nat,
                         dt: DataType,
                         a: Phrase[AccType],
                         i: Identifier[ExpType],
                         o: Phrase[AccType],
                         p: Phrase[CommType],
                         env: Environment): Stmt = {
      val cI = C.AST.DeclRef(freshName("i_"))
      val range = RangeAdd(0, n, 1)
      val updatedGen = updatedRanges(cI.name, range)

      val init = C.AST.VarDecl(cI.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0)))
      val cond = C.AST.BinaryExpr(cI, C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n))
      val increment = shine.C.AST.Assignment(cI, C.AST.ArithmeticExpr(NamedVar(cI.name, range) + 1))

      Phrase.substitute(a `@v` i, `for` = o, `in` = p) |> (p =>

      env.updatedIdentEnv(i -> cI) |> (env =>

      C.AST.Stmts(
        C.AST.Code("#pragma omp simd"),
        C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
          C.AST.Block(immutable.Seq(p |> updatedGen.cmd(env)))))))
    }

    def codeGenLiteral(d: Data): Expr = {
      d match {
        case VectorData(vector) => CCodeGen.codeGenLiteral(ArrayData(vector))
        case _ => CCodeGen.codeGenLiteral(d)
      }
    }

    def codeGenForeignFunctionCall(funDecl: rise.core.ForeignFunction.Decl,
                                   inTs: collection.Seq[DataType],
                                   outT: DataType,
                                   args: collection.Seq[Phrase[ExpType]],
                                   env: Environment,
                                   ps: Path,
                                   cont: Expr => Stmt): Stmt = {
      (outT, ps) match {
        case (_: ScalarType, Nil) =>
          CCodeGen.codeGenForeignFunctionCall(funDecl, inTs, outT, args, env, cont)

        // TODO: This has to be generalised at some point ...
        case (VectorType(_, elemType), i :: Nil) =>
          // this is not really generic, to treat all arguments the same ...
          val inTs_ = inTs.map {
            case VectorType(_, et) => et
            case _ => throw new Exception("This should not happen")
          }
          funDecl.definition match {
            case Some(funDef) =>
              addDeclaration(
                C.AST.FunDecl(funDecl.name,
                  returnType = typ(elemType),
                  params = (funDef.params zip inTs_).map {
                    case (name, dt) => C.AST.ParamDecl(name, typ(dt))
                  },
                  body = C.AST.Code(funDef.body)
                )
              )
            case _ =>
          }

          CCodeGen.codeGenForeignCall(funDecl.name, args, env, i :: Nil, cont)

        case _ =>
          throw new Exception(s"Can not generate fun call to $funDecl with current path $ps")
      }
    }
  }
}
