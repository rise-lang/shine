package AccPatterns

import Core.OperationalSemantics._
import Core._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.VarRef

import scala.xml.Elem

case class ToGlobalAcc(dt: DataType,
                       p: Phrase[AccType]) extends AccPattern{

  override lazy val `type` = acc"[$dt]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    p checkType acc"[$dt]"
  }

  override def eval(s: Store): AccIdentifier = ???

  override def toOpenCL(env: ToOpenCL.Environment): VarRef = ???

  def toOpenCL(env: ToOpenCL.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr], dt: DataType): VarRef = ???

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[AccType] = {
    ToGlobalAcc(fun(dt), VisitAndRebuild(p, fun))
  }

  override def prettyPrint: String = s"(toGlobalAcc ${PrettyPrinter(p)})"

  override def xmlPrinter: Elem =
    <toGlobalAcc>
      {Core.xmlPrinter(p)}
    </toGlobalAcc>
}
