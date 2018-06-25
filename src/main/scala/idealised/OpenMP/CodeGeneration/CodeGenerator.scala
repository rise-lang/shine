package idealised.OpenMP.CodeGeneration

import idealised.C
import idealised.C.AST.{ArraySubscript, Decl}
import idealised.C.CodeGeneration.{CodeGenerator => CCodeGenerator}
import idealised.DPIA.{Nat, error, freshName}
import idealised.DPIA.Phrases.{Identifier, Phrase}
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType, VectorType}
import idealised.DPIA.DSL._
import idealised.OpenCL.FunctionalPrimitives.{AsScalar, AsVector}
import idealised.OpenCL.ImperativePrimitives.{AsScalarAcc, AsVectorAcc}
import lift.arithmetic
import lift.arithmetic._

import scala.collection.{immutable, mutable}

object CodeGenerator {
  def apply(p: Phrase[CommandType], env: CCodeGenerator.Environment): CodeGenerator =
    new CodeGenerator(p, env, mutable.ListBuffer[Decl](), immutable.Map[String, arithmetic.Range]())
}

class CodeGenerator(override val p: Phrase[CommandType],
                    override val env: CCodeGenerator.Environment,
                    override val decls: CCodeGenerator.Declarations,
                    override val ranges: CCodeGenerator.Ranges)
  extends CCodeGenerator(p, env, decls, ranges)
{

  override def updatedRanges(key: String, value: lift.arithmetic.Range): CodeGenerator =
    new CodeGenerator(p, env, decls, ranges.updated(key, value))

  override def acc(phrase: Phrase[AccType], env: Environment, path: Path): Expr = {
    phrase match {
      case AsVectorAcc(n, _, _, a) => path match {
        case i :: ps =>     acc(a, env, (i / n) :: ps)
        case _ =>           error(s"Expected path to be not empty")
      }
      case AsScalarAcc(_, m, dt, a) => path match {
        case i :: j :: ps =>
          acc(a, env, (i * m) + j :: ps)

        case i :: Nil =>
          acc(a, env, (i * m) :: Nil) match {
            case ArraySubscript(v, idx) =>
              // emit something like: ((struct float4 *)v)[idx]
              val ptrType = C.AST.PointerType(C.AST.Type.fromDataType(VectorType(m, dt)))
              C.AST.ArraySubscript(C.AST.Cast(ptrType, v), idx)
          }
        case _ =>           error(s"Expected path to be not empty")
      }
      case _ =>             super.acc(phrase, env, path)
    }
  }

  override def exp(phrase: Phrase[ExpType], env: Environment, path: Path): Expr = {
    phrase match {
      case AsVector(n, _, dt, e) => path match {
        case i :: j :: ps =>
          exp(e, env, (i * n) + j :: ps)

        case i :: Nil =>
          exp(e, env, (i * n) :: Nil) match {
            case ArraySubscript(v, idx) =>
              // emit something like: ((struct float4 *)v)[idx]
              val ptrType = C.AST.PointerType(C.AST.Type.fromDataType(VectorType(n, dt)))
              C.AST.ArraySubscript(C.AST.Cast(ptrType, v), idx)
          }
        case _ =>           error(s"Expected path to be not empty")
      }
      case AsScalar(_, m, _, e) => path match {
        case i :: ps =>     exp(e, env, (i / m) :: ps)
        case _ =>           error(s"Expected path to be not empty")
      }
      case _ =>             super.exp(phrase, env, path)
    }
  }

  override def codeGenParFor(n: Nat,
                             dt: DataType,
                             a: Phrase[AccType],
                             i: Identifier[ExpType],
                             o: Phrase[AccType],
                             p: Phrase[CommandType],
                             env: Environment,
                             gen: CodeGenerator.this.type): Stmt = {
    val i_ = C.AST.DeclRef(freshName("i_"))
    val range = RangeAdd(0, n, 1)
    val updatedGen = gen.updatedRanges(i_.name, range)

    val init = C.AST.VarDecl(i_.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0)))
    val cond = C.AST.BinaryExpr(i_, C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n))
    val increment = idealised.C.AST.Assignment(i_, C.AST.ArithmeticExpr(NamedVar(i_.name, range) + 1))

    C.AST.Stmts(
      C.AST.Code("#pragma omp parallel for"),
      C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
        C.AST.Block(immutable.Seq(updatedGen.cmd(Phrase.substitute(a `@` i, `for`=o, `in`=p), env updatedIdentEnv (i -> i_))))))
  }

  override def codeGenParForVec(n: Nat,
                                dt: DataType,
                                a: Phrase[AccType],
                                i: Identifier[ExpType],
                                o: Phrase[AccType],
                                p: Phrase[CommandType],
                                env: Environment,
                                gen: CodeGenerator.this.type): Stmt = {
    val i_ = C.AST.DeclRef(freshName("i_"))
    val range = RangeAdd(0, n, 1)
    val updatedGen = gen.updatedRanges(i_.name, range)

    val init = C.AST.VarDecl(i_.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0)))
    val cond = C.AST.BinaryExpr(i_, C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n))
    val increment = idealised.C.AST.Assignment(i_, C.AST.ArithmeticExpr(NamedVar(i_.name, range) + 1))

    C.AST.Stmts(
      C.AST.Code("#pragma omp simd"),
      C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
        C.AST.Block(immutable.Seq(updatedGen.cmd(Phrase.substitute(a `@v` i, `for`=o, `in`=p), env updatedIdentEnv (i -> i_))))))
  }
}
