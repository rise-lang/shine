package idealised.OpenMP.CodeGeneration

import idealised.C
import idealised.C.AST.{ArraySubscript, Decl}
import idealised.C.CodeGeneration.{CodeGenerator => CCodeGenerator}
import idealised.DPIA.{Nat, error, freshName}
import idealised.DPIA.Phrases.{Identifier, Lambda, NatDependentLambda, Phrase}
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType, PhraseType, VectorType}
import idealised.DPIA.DSL._
import idealised.DPIA.ImperativePrimitives.{ParFor, ParForNat, ParForVec}
import idealised.OpenCL.FunctionalPrimitives.{AsScalar, AsVector}
import idealised.OpenCL.ImperativePrimitives.{AsScalarAcc, AsVectorAcc}
import idealised.SurfaceLanguage.NatIdentifier
import lift.arithmetic
import lift.arithmetic._

import scala.collection.{immutable, mutable}

object CodeGenerator {
  def apply(env: CCodeGenerator.Environment): CodeGenerator =
    new CodeGenerator(env, mutable.ListBuffer[Decl](), immutable.Map[String, arithmetic.Range]())
}

class CodeGenerator(override val env: CCodeGenerator.Environment,
                    override val decls: CCodeGenerator.Declarations,
                    override val ranges: CCodeGenerator.Ranges)
  extends CCodeGenerator(env, decls, ranges)
{
  override def name: String = "OpenMP"

  override def updatedRanges(key: String, value: lift.arithmetic.Range): CodeGenerator =
    new CodeGenerator(env, decls, ranges.updated(key, value))


  override def cmd(phrase: Phrase[CommandType], env: Environment): Stmt = {
    phrase match {
      case ParFor(n, dt, a, Lambda(i, Lambda(o, p))) => codeGenParFor(n, dt, a, i, o, p, env)
      case ParForVec(n, dt, a, Lambda(i, Lambda(o, p))) => codeGenParForVec(n, dt, a, i, o, p, env)
      case ParForNat(n, i_dt, dt, a, NatDependentLambda(i, Lambda(o, p))) => codeGenParForNat(n, i_dt, dt, a, i, o, p, env)
      case _ => super.cmd(phrase, env)
    }
  }

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
              val ptrType = C.AST.PointerType(typ(VectorType(m, dt)))
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
              val ptrType = C.AST.PointerType(typ(VectorType(n, dt)))
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

  private def codeGenParFor(n: Nat,
                            dt: DataType,
                            a: Phrase[AccType],
                            i: Identifier[ExpType],
                            o: Phrase[AccType],
                            p: Phrase[CommandType],
                            env: Environment): Stmt = {
    val i_ = C.AST.DeclRef(freshName("i_"))
    val range = RangeAdd(0, n, 1)
    val updatedGen = updatedRanges(i_.name, range)

    val init = C.AST.VarDecl(i_.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0)))
    val cond = C.AST.BinaryExpr(i_, C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n))
    val increment = idealised.C.AST.Assignment(i_, C.AST.ArithmeticExpr(NamedVar(i_.name, range) + 1))

    C.AST.Stmts(
      C.AST.Code("#pragma omp parallel for"),
      C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
        C.AST.Block(immutable.Seq(updatedGen.cmd(Phrase.substitute(a `@` i, `for`=o, `in`=p), env updatedIdentEnv (i -> i_))))))
  }

  private def codeGenParForNat(
                                n: Nat,
                                i_dt:NatIdentifier,
                                dt: DataType,
                                a: Phrase[AccType],
                                i: NatIdentifier,
                                o: Phrase[AccType],
                                p: Phrase[CommandType],
                                env: Environment): Stmt = {

    val i_ = C.AST.DeclRef(freshName("i_"))
    val range = RangeAdd(0, n, 1)
    val updatedGen = updatedRanges(i_.name, range)

    val init = C.AST.VarDecl(i_.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0)))
    val cond = C.AST.BinaryExpr(i_, C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n))
    val increment = idealised.C.AST.Assignment(i_, C.AST.ArithmeticExpr(NamedVar(i_.name, range) + 1))

    val pSub = PhraseType.substitute(NamedVar(i_.name, range), `for` = i, in = p)

    C.AST.Stmts(
      C.AST.Code("#pragma omp parallel for"),
      C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
        C.AST.Block(immutable.Seq(updatedGen.cmd(Phrase.substitute(a `@d` i, `for`=o, `in`=pSub), env)))))
  }

  private def codeGenParForVec(n: Nat,
                               dt: DataType,
                               a: Phrase[AccType],
                               i: Identifier[ExpType],
                               o: Phrase[AccType],
                               p: Phrase[CommandType],
                               env: Environment): Stmt = {
    val i_ = C.AST.DeclRef(freshName("i_"))
    val range = RangeAdd(0, n, 1)
    val updatedGen = updatedRanges(i_.name, range)

    val init = C.AST.VarDecl(i_.name, C.AST.Type.int, init = Some(C.AST.ArithmeticExpr(0)))
    val cond = C.AST.BinaryExpr(i_, C.AST.BinaryOperator.<, C.AST.ArithmeticExpr(n))
    val increment = idealised.C.AST.Assignment(i_, C.AST.ArithmeticExpr(NamedVar(i_.name, range) + 1))

    C.AST.Stmts(
      C.AST.Code("#pragma omp simd"),
      C.AST.ForLoop(C.AST.DeclStmt(init), cond, increment,
        C.AST.Block(immutable.Seq(updatedGen.cmd(Phrase.substitute(a `@v` i, `for`=o, `in`=p), env updatedIdentEnv (i -> i_))))))
  }
}
