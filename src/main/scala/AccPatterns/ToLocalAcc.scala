package AccPatterns

import Core.OperationalSemantics._
import Core.PrettyPrinter.Indent
import Core._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.VarRef

case class ToLocalAcc(p: Phrase[AccType]) extends AccPattern{

  override def typeCheck(): AccType = {
    import TypeChecker._
    TypeChecker(p) match {
      case AccType(dt) => AccType(dt)
      case x => error(x.toString, "AccType")
    }
  }

  override def eval(s: Store): AccIdentifier = ???

  override def toOpenCL(ocl: ToOpenCL): VarRef = ???

  def toOpenCL(ocl: ToOpenCL, arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): VarRef = ???

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[AccType] = {
    ToLocalAcc(VisitAndRebuild(p, fun))
  }

  override def prettyPrint(indent: Indent): String =
    indent + s"(toLocalAcc\n" +
      s"${PrettyPrinter(p, indent.more)}" +
      indent + s"\n)"
}
