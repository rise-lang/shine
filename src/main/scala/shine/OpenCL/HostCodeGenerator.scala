package shine.OpenCL

import arithexpr.arithmetic
import shine.C
import shine.C.AST._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.OpenCL.primitives.imperative.KernelCallCmd

import scala.collection.{immutable, mutable}

object HostCodeGenerator {
  def apply(): HostCodeGenerator =
    new HostCodeGenerator(mutable.ListBuffer[Decl](), immutable.Map[String, arithmetic.Range]())
}

case class HostCodeGenerator(override val decls: C.CodeGenerator.Declarations,
                             override val ranges: C.CodeGenerator.Ranges)
  extends C.CodeGenerator(decls, ranges)
{
  override def name: String = "OpenCL Host"

  override def cmd(env: Environment): Phrase[CommType] => Stmt = {
    case KernelCallCmd(name, LocalSize(ls), GlobalSize(gs), _, _, output, args) =>
      output |> acc(env, Nil, output =>
        expSeq(args, env, args =>
          C.AST.ExprStmt(C.AST.FunCall(C.AST.DeclRef(name),
            NDRangeToAST(ls) ++ NDRangeToAST(gs) ++ Seq(output) ++ args))))
    case phrase => phrase |> super.cmd(env)
  }

  private def NDRangeToAST(r: NDRange): Seq[Expr] =
    Seq(C.AST.ArithmeticExpr(r.x), C.AST.ArithmeticExpr(r.y), C.AST.ArithmeticExpr(r.z))

  private def expSeq(ps: collection.Seq[Phrase[ExpType]],
                     env: Environment,
                     k: collection.Seq[Expr] => Stmt): Stmt = {
    def iter(ps: collection.Seq[Phrase[ExpType]], res: mutable.ArrayBuffer[Expr]): Stmt =
      ps match {
        case p +: ps => p |> exp(env, Nil, e => iter(ps, res += e))
        case _ => k(res)
      }

    iter(ps, new mutable.ArrayBuffer[Expr]())
  }
}