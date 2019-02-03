package idealised.OpenCL.CodeGeneration

import idealised.C.AST.{ArraySubscript, Decl}
import idealised.C.CodeGeneration.{CodeGenerator => CCodeGenerator}
import idealised.DPIA.FunctionalPrimitives.{AsScalar, AsVector}
import idealised.DPIA.ImperativePrimitives.{AsScalarAcc, AsVectorAcc, ForNat}
import idealised.DPIA.Phrases._
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType, PhraseType, VectorType}
import idealised.DPIA.{Nat, NatIdentifier, Phrases, error, freshName}
import idealised.OpenCL.ImperativePrimitives.OpenCLParFor
import idealised.OpenMP.ImperativePrimitives.ParForNat
import idealised.{C, OpenCL}
import lift.arithmetic
import lift.arithmetic._
import idealised.DPIA.DSL._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics.{ArrayData, VectorData}

import scala.collection.{immutable, mutable}

object CodeGenerator {
  def apply(localSize: Nat, globalSize: Nat): CodeGenerator =
    new CodeGenerator(mutable.ListBuffer[Decl](), immutable.Map[String, arithmetic.Range](), localSize, globalSize)
}

class CodeGenerator(override val decls: CCodeGenerator.Declarations,
                    override val ranges: CCodeGenerator.Ranges,
                    localSize: Nat,
                    globalSize: Nat)
  extends CCodeGenerator(decls, ranges)
{
  override def name: String = "OpenCL"

  override def updatedRanges(key: String, value: lift.arithmetic.Range): CodeGenerator =
    new CodeGenerator(decls, ranges.updated(key, value), localSize, globalSize)

  override def cmd(phrase: Phrase[CommandType], env: Environment): Stmt = {
    phrase match {
      case f@OpenCLParFor(n, dt, a, Lambda(i, Lambda(o, p))) => OpenCLCodeGen.codeGenOpenCLParFor(f, n, dt, a, i, o, p, env)

      case ParForNat(n, i, _, out, body) =>
        val newBody = body(i)(out `@d` i)

        //In new body, all body.t.x variables (the nat identifier) need to be substituted with i
        val newIdentEnv = env.identEnv.map {
          case (Identifier(name, AccType(dt)), declRef) =>
            (Identifier(name, AccType(DataType.substitute(i, `for` = body.t.x, in=dt))), declRef)
          case (Identifier(name, ExpType(dt)), declRef) =>
            (Identifier(name, ExpType(DataType.substitute(i, `for` = body.t.x, in=dt))), declRef)
          case x => x
        }

        OpenCLCodeGen.codeGenForNat(n, i, newBody, env.copy(identEnv = newIdentEnv))

      case ForNat(n, NatDependentLambda(i, p)) => OpenCLCodeGen.codeGenForNat(n, i, p, env)

      case _ => super.cmd(phrase, env)
    }
  }

  override def acc(phrase: Phrase[AccType], env: Environment, path: Path): Expr = {
    phrase match {
      case AsVectorAcc(_, _, _, _) => ???
      case AsScalarAcc(_, m, dt, a) => ???

      case _ => super.acc(phrase, env, path)
    }
  }

  override def exp(phrase: Phrase[ExpType], env: Environment, path: Path): Expr = {
    phrase match {
      case Phrases.Literal(n) => (path, n.dataType) match {
        case (Nil, _: VectorType) => OpenCLCodeGen.codeGenLiteral(n)
        case (i :: Nil, _: VectorType) => ???
        case _ => super.exp(phrase, env, path)
      }
      case UnaryOp(op, e) => phrase.t.dataType match {
        case _: VectorType => path match {
          case i :: ps => CCodeGen.codeGenUnaryOp(op, exp(e, env, i :: ps))
          case _ => error(s"Expected path to be not empty")
        }
        case _ => super.exp(phrase, env, path)
      }
      case BinOp(op, e1, e2) => phrase.t.dataType match {
        case _: VectorType => path match {
          case Nil => CCodeGen.codeGenBinaryOp(op, exp(e1, env, Nil), exp(e2, env, Nil))
          case i :: ps => CCodeGen.codeGenBinaryOp(op, exp(e1, env, i :: ps), exp(e2, env, i :: ps))
          case _ => error(s"Expected path to be not empty")
        }
        case _ => super.exp(phrase, env, path)
      }
      case AsVector(n, _, dt, e) => path match {
        case i :: j :: ps => exp(e, env, (i * n) + j :: ps)
        case i :: Nil =>
          exp(e, env, (i * n) :: Nil) match {
            case ArraySubscript(v, idx) =>
              // emit a vload
              C.AST.Comment("vload")
              // in OpenMP something like: ((struct float4 *)v)[idx] is emitted
              val ptrType = C.AST.PointerType(typ(VectorType(n, dt)))
              C.AST.ArraySubscript(C.AST.Cast(ptrType, v), idx)
          }
        case Nil => error(s"Expected path to have two elements")
      }
      case AsScalar(_, _, _, _) => ???

      case _ => super.exp(phrase, env, path)
    }
  }


  override def typ(dt: DataType): Type = dt match {
    case _: idealised.DPIA.Types.VectorType => C.AST.BasicType("float4")
    case _ => super.typ(dt)
  }

  protected object OpenCLCodeGen {
    def codeGenOpenCLParFor(f: OpenCLParFor,
                            n: Nat,
                            dt: DataType,
                            a: Phrase[AccType],
                            i: Identifier[ExpType],
                            o: Phrase[AccType],
                            p: Phrase[CommandType],
                            env: Environment): Stmt = {
      val i_ = C.AST.DeclRef(f.name)
      val range = RangeAdd(f.init, n, f.step)
      val updatedGen = updatedRanges(i_.name, range)

      val n_ = applySubstitutions(n, env.identEnv)

      range.numVals match {
        // iteration count is 0 => skip body; no code to be emitted
        case Cst(0) => C.AST.Comment("iteration count is 0, no loop emitted")

        // iteration count is 1 => no loop
        case Cst(1) =>
          C.AST.Stmts(C.AST.Stmts(
            C.AST.Comment("iteration count is exactly 1, no loop emitted"),
            C.AST.DeclStmt(C.AST.VarDecl(i_.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0))))),
            updatedGen.cmd(p, env updatedIdentEnv (i -> i_)))

        case _ =>
          // default case
          val init = OpenCL.AST.VarDecl(i_.name, C.AST.Type.int, OpenCL.PrivateMemory, init = Some(C.AST.ArithmeticExpr(f.init)))
          val cond = C.AST.BinaryExpr(i_, C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n_))
          val increment = C.AST.Assignment(i_, C.AST.ArithmeticExpr(NamedVar(i_.name, range) + f.step))

          C.AST.Stmts(
            C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
              C.AST.Block(immutable.Seq(updatedGen.cmd(p, env updatedIdentEnv (i -> i_))))),
            f.synchronize
          )
      }
    }

    def codeGenForNat(n: Nat,
                      i: NatIdentifier,
                      p: Phrase[CommandType],
                      env: Environment): Stmt = {
      val i_ = C.AST.DeclRef(freshName("i_"))
      val range = RangeAdd(0, n, 1)
      val updatedGen = updatedRanges(i_.name, range)

      val n_ = applySubstitutions(n, env.identEnv)

      range.numVals match {
        // iteration count is 0 => skip body; no code to be emitted
        case Cst(0) => C.AST.Comment("iteration count is 0, no loop emitted")

        // iteration count is 1 => no loop
        case Cst(1) =>
          C.AST.Stmts(C.AST.Stmts(
            C.AST.Comment("iteration count is exactly 1, no loop emitted"),
            C.AST.DeclStmt(C.AST.VarDecl(i_.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0))))),
            updatedGen.cmd(p, env))

        case _ =>
          // default case
          val init = C.AST.VarDecl(i_.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0)))
          val cond = C.AST.BinaryExpr(i_, C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n_))
          val increment = C.AST.Assignment(i_, C.AST.ArithmeticExpr(NamedVar(i_.name, range) + 1))

          val pSub = PhraseType.substitute(NamedVar(i_.name, range), `for` = i, in = p)
          // we need this for OpenCL, as nested allocations are lifted into the environment
          // TODO: rethink if this is required when addressing the issue of hoisting the memory
          val newIdentEnv = env.identEnv.map {
            case (Identifier(name, AccType(dt)), declRef) =>
              (Identifier(name, AccType(DataType.substitute(NamedVar(i_.name, range), `for` = i, in = dt))), declRef)
            case (Identifier(name, ExpType(dt)), declRef) =>
              (Identifier(name, ExpType(DataType.substitute(NamedVar(i_.name, range), `for` = i, in = dt))), declRef)
            case x => x
          }

          C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
            C.AST.Block(immutable.Seq(updatedGen.cmd(pSub, env.copy(identEnv = newIdentEnv)))))
      }
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
