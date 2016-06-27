package AccPatterns

import Core.OperationalSemantics._
import Core._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.VarRef

import scala.xml.Elem

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

  override def prettyPrint: String = s"(toLocalAcc ${PrettyPrinter(p)})"

  override def xmlPrinter: Elem =
    <toLocalAcc>
      {Core.xmlPrinter(p)}
    </toLocalAcc>
}
