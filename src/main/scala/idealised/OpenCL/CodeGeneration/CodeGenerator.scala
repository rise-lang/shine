package idealised.OpenCL.CodeGeneration

import idealised.C.AST.{ArraySubscript, BasicType, Decl}
import idealised.C.CodeGeneration.CodeGenerator.CIntExpr
import idealised.C.CodeGeneration.{CodeGenerator => CCodeGenerator}
import idealised.DPIA.FunctionalPrimitives._
import idealised.DPIA.ImperativePrimitives._
import idealised.DPIA.Phrases._
import idealised.DPIA.DSL._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics.VectorData
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.FunctionalPrimitives.OpenCLFunction
import idealised.OpenCL.ImperativePrimitives._
import idealised.OpenCL._
import idealised._
import lift.arithmetic
import lift.arithmetic._

import scala.collection.{immutable, mutable}

object CodeGenerator {
  def apply(localSize: Option[NDRange], globalSize: Option[NDRange]): CodeGenerator =
    new CodeGenerator(mutable.ListBuffer[Decl](), immutable.Map[String, arithmetic.Range](), localSize, globalSize)
}

class CodeGenerator(override val decls: CCodeGenerator.Declarations,
                    override val ranges: CCodeGenerator.Ranges,
                    //TODO Use information about sizes. Sizes are currently not used in the CodeGenerator.
                    localSize: Option[NDRange],
                    globalSize: Option[NDRange])
  extends CCodeGenerator(decls, ranges) {
  override def name: String = "OpenCL"

  override def updatedRanges(key: String, value: lift.arithmetic.Range): CodeGenerator =
    new CodeGenerator(decls, ranges.updated(key, value), localSize, globalSize)

  override def cmd(phrase: Phrase[CommandType], env: Environment): Stmt = {
    phrase match {
      case f@OpenCLParFor(n, dt, a, Lambda(i, Lambda(o, p))) =>
        OpenCLCodeGen.codeGenOpenCLParFor(f, n, dt, a, i, o, p, env)

      case f@OpenCLParForNat(n, _, a, NatDependentLambda(i, Lambda(o, p))) =>
        OpenCLCodeGen.codeGenOpenCLParForNat(f, n, a, i, o, p, env)

      case Assign(dt, a, e) => dt match {
        case VectorType(_, _) =>
          //noinspection VariablePatternShadow
          exp(e, env, Nil, e =>
            acc(a, env, Nil, {
              case C.AST.FunCall(C.AST.DeclRef(name), immutable.Seq(idx, v))
                if name.startsWith("vstore") =>
                C.AST.ExprStmt(C.AST.FunCall(C.AST.DeclRef(name), immutable.Seq(e, idx, v)))
              case a => C.AST.ExprStmt(C.AST.Assignment(a, e))
            }))

        case _ => super.cmd(phrase, env)
      }

      case OpenCLNew(dt, addrSpace, Lambda(v, p)) => OpenCLCodeGen.codeGenOpenCLNew(dt, addrSpace, v, p, env)

      case _: New => throw new Exception("New without address space found in OpenCL program.")

      case _ => super.cmd(phrase, env)
    }
  }

  override def acc(phrase: Phrase[AccType],
                   env: Environment,
                   path: Path,
                   cont: Expr => Stmt): Stmt = {
    phrase match {
      case AsVectorAcc(n, _, _, a) => path match {
        case (i : CIntExpr) :: ps =>     acc(a, env, CIntExpr(i / n) :: ps, cont)
        case _ =>           error(s"Expected path to be not empty")
      }
      case AsScalarAcc(_, m, dt, a) => path match {
        case (i : CIntExpr) :: (j : CIntExpr) :: ps =>
          acc(a, env, CIntExpr((i * m) + j) :: ps, cont)

        case (i : CIntExpr) :: Nil =>

          acc(a, env, CIntExpr(i * m) :: Nil, {
            case ArraySubscript(v, idx) =>
              // the continuation has to add the value ...
              cont( C.AST.FunCall(C.AST.DeclRef(s"vstore$m"), immutable.Seq(idx, v)) )
          })
        case _ =>           error(s"Expected path to be not empty")
      }
      case IdxVecAcc(_, _, i, a) => CCodeGen.codeGenIdxAcc(i, a, env, path, cont)
      case _ => super.acc(phrase, env, path, cont)
    }
  }

  override def exp(phrase: Phrase[ExpType],
                   env: Environment,
                   path: Path,
                   cont: Expr => Stmt): Stmt = {
    phrase match {
      case Phrases.Literal(n) => (path, n.dataType) match {
        case (Nil, _: VectorType)       => cont( OpenCLCodeGen.codeGenLiteral(n) )
        case (i :: Nil, _: VectorType)  => ???
        case _ => super.exp(phrase, env, path, cont)
      }
      case UnaryOp(op, e) => phrase.t.dataType match {
        case _: VectorType => path match {
          case i :: ps => exp(e, env, i :: ps, e =>
            cont(CCodeGen.codeGenUnaryOp(op, e)))
          case _ => error(s"Expected path to be not empty")
        }
        case _ => super.exp(phrase, env, path, cont)
      }
      case BinOp(op, e1, e2) => phrase.t.dataType match {
        case _: VectorType =>
          exp(e1, env, path, e1 =>
            exp(e2, env, path, e2 =>
              cont(CCodeGen.codeGenBinaryOp(op, e1, e2))
          ))
        case _ => super.exp(phrase, env, path, cont)
      }
      case AsVector(n, _, _, e) => path match {
        case (i : CIntExpr) :: (j : CIntExpr) :: ps => exp(e, env, CIntExpr((i * n) + j) :: ps, cont)
        case (i : CIntExpr) :: Nil =>
          exp(e, env, CIntExpr(i * n) :: Nil, {
            case ArraySubscript(v, idx) =>
              // TODO: check that idx is the right offset ...
              cont( C.AST.FunCall(C.AST.DeclRef(s"vload$n"), immutable.Seq(idx, v)) )
          })
        case Nil => error(s"Expected path to have two elements")
      }
      case AsScalar(_, m, _, e) => path match {
        case (i : CIntExpr) :: ps =>     exp(e, env, CIntExpr(i / m) :: ps, cont)
        case _ =>           error(s"Expected path to be not empty")
      }
      // TODO: this has to be refactored
      case VectorFromScalar(n, st, e) => path match {
        case (_: CIntExpr) :: ps =>
          // in this case we index straight into the vector build from a single scalar
          // it is equivalent to return the scalar `e' without boxing and unboxing it
          exp(e, env, ps, cont)

        case Nil =>
          exp(e, env, Nil, e =>
            cont(C.AST.Literal(s"($st$n)(" + C.AST.Printer(e) + ")")))
      }

      case IdxVec(_, _, i, e) => CCodeGen.codeGenIdx(i, e, env, path, cont)

      case OpenCLFunction(name, _, _, args) =>
        CCodeGen.codeGenForeignCall(name, args, env, Nil, cont)

      case _ => super.exp(phrase, env, path, cont)
    }
  }

  override def typ(dt: DataType): Type = dt match {
    case VectorType(n, elemType) =>
      OpenCL.AST.VectorType(n, super.typ(elemType).asInstanceOf[BasicType])
    case _ => super.typ(dt)
  }

  override def genNat(n: Nat, env: Environment): Expr = n match {
    case of: BuiltInFunction => C.AST.Literal(of.toOCLString)
    case _ => super.genNat(n, env)
  }

  protected object OpenCLCodeGen {
    def codeGenOpenCLNew(dt: DataType,
                   addressSpace: AddressSpace,
                   v: Identifier[VarType],
                   p: Phrase[CommandType],
                   env: Environment): Stmt = {
      val ve = Identifier(s"${v.name}_e", v.t.t1)
      val va = Identifier(s"${v.name}_a", v.t.t2)
      val vC = C.AST.DeclRef(v.name)

      C.AST.Block(immutable.Seq(
        C.AST.DeclStmt(OpenCL.AST.VarDecl(vC.name, typ(dt), addressSpace)),
        cmd(Phrase.substitute(Pair(ve, va), `for` = v, `in` = p),
          env updatedIdentEnv (ve -> vC)
            updatedIdentEnv (va -> vC))))
    }

    def codeGenOpenCLParFor(f: OpenCLParFor,
                            n: Nat,
                            dt: DataType,
                            a: Phrase[AccType],
                            i: Identifier[ExpType],
                            o: Phrase[AccType],
                            p: Phrase[CommandType],
                            env: Environment): Stmt = {
      val cI = C.AST.DeclRef(f.name)
      val range = RangeAdd(f.init, n, f.step)
      val updatedGen = updatedRanges(cI.name, range)

      applySubstitutions(n, env.identEnv) |> (n => {

      val init = OpenCL.AST.VarDecl(cI.name, C.AST.Type.int, OpenCL.PrivateMemory, init = Some(C.AST.ArithmeticExpr(f.init)))
      val cond = C.AST.BinaryExpr(cI, C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n))
      val increment = C.AST.Assignment(cI, C.AST.ArithmeticExpr(NamedVar(cI.name, range) + f.step))

      Phrase.substitute(a `@` i, `for` = o, `in` = p) |> (p =>

      env.updatedIdentEnv(i -> cI) |> (env => {

        val test = range.numVals
        val min = test.min
        val max = test.max
      range.numVals match {
        // iteration count is 0 => skip body; no code to be emitted
        case Cst(0) => C.AST.Comment("iteration count is 0, no loop emitted")
        // iteration count is 1 => no loop
        case Cst(1) =>
          C.AST.Stmts(C.AST.Stmts(
            C.AST.Comment("iteration count is exactly 1, no loop emitted"),
            C.AST.DeclStmt(C.AST.VarDecl(cI.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0))))),
            updatedGen.cmd(p, env))
        case _ if range.numVals.min == NegInf && range.numVals.max == Cst(1) =>
          C.AST.Stmts(
            C.AST.DeclStmt(init),
            C.AST.IfThenElse(cond, updatedGen.cmd(p, env) , None)
          )
        // default case
        case _ =>
          C.AST.Stmts(
            C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
              C.AST.Block(immutable.Seq(updatedGen.cmd(p, env)))),
            f.synchronize)
      }}))})
    }

    def codeGenOpenCLParForNat(f: OpenCLParForNat,
                               n: Nat,
                               a: Phrase[AccType],
                               i: NatIdentifier,
                               o: Phrase[AccType],
                               p: Phrase[CommandType],
                               env: Environment): Stmt = {
      val cI = C.AST.DeclRef(f.name)
      val range = RangeAdd(f.init, n, f.step)
      val updatedGen = updatedRanges(cI.name, range)

      applySubstitutions(n, env.identEnv) |> (n => {

        val init = OpenCL.AST.VarDecl(cI.name, C.AST.Type.int, OpenCL.PrivateMemory, init = Some(C.AST.ArithmeticExpr(f.init)))
        val cond = C.AST.BinaryExpr(cI, C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n))
        val increment = C.AST.Assignment(cI, C.AST.ArithmeticExpr(NamedVar(cI.name, range) + f.step))

        // FIRST we must substitute in the indexing of o in the phrase
        Phrase.substitute(a `@d` i, `for` = o, `in` = p) |> (p =>
          // THEN and only THEN we can change the type to use the new index var
          PhraseType.substitute(NamedVar(cI.name, range), `for` = i, in = p) |> (p =>

            env.copy(identEnv = env.identEnv.map {
              case (Identifier(name, AccType(dt)), declRef) =>
                (Identifier(name, AccType(DataType.substitute(NamedVar(cI.name, range), `for` = i, in = dt))), declRef)
              case (Identifier(name, ExpType(dt)), declRef) =>
                (Identifier(name, ExpType(DataType.substitute(NamedVar(cI.name, range), `for` = i, in = dt))), declRef)
              case x => x
            }) |> (env =>

              range.numVals match {
                // iteration count is 0 => skip body; no code to be emitted
                case Cst(0) => C.AST.Comment("iteration count is 0, no loop emitted")
                  // TODO: substitute a `@d` Cst(0) earlier ...
//                // iteration count is 1 => no loop
//                case Cst(1) =>
//                  C.AST.Stmts(C.AST.Stmts(
//                    C.AST.Comment("iteration count is exactly 1, no loop emitted"),
//                    C.AST.DeclStmt(C.AST.VarDecl(cI.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0))))),
//                    updatedGen.cmd(p, env))
                // default case
                case _ =>
                  C.AST.Stmts(
                    C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
                      C.AST.Block(immutable.Seq(updatedGen.cmd(p, env)))),
                    f.synchronize)
              })))})
    }

    def codeGenLiteral(d: OperationalSemantics.Data): Expr = {
      d match {
        case VectorData(vector) => d.dataType match {
          case VectorType(n, elemType) =>
            C.AST.Literal("(" + s"($elemType$n)" + vector.mkString("{", ",", "}") + ")")
          case _ => error("This should not happen")
        }
        case _ => CCodeGen.codeGenLiteral(d)
      }
    }
  }

}
