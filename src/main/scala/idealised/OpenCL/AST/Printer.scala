package idealised.OpenCL.AST

import idealised.C.AST._
import lift.arithmetic.ArithExpr
import opencl.generator.OclFunction

object Printer {
  def apply(n: Node): String = (new Printer).printNode(n)
}

class Printer extends idealised.C.AST.CPrinter {
  override def printDecl(d: Decl): Unit = super.printDecl(d)

  override def printExpr(e: Expr): Unit = super.printExpr(e)

  override def printStmt(s: Stmt): Unit = s match {
    case _: Barrier => print("barrier();")
    case _ => super.printStmt(s)
  }

  override def typeName(t: Type): String = super.typeName(t)

  override def toString(e: ArithExpr): String = e match {
    case of: OclFunction => of.toOCLString

    case _ => super.toString(e)
  }
}
