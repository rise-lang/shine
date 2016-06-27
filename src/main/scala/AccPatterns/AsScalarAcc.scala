package AccPatterns

import Core._
import Core.OperationalSemantics._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.VarRef

import scala.xml.Elem

case class AsScalarAcc(n: ArithExpr,
                       array: Phrase[AccType])
  extends AccPattern {

  override def typeCheck(): AccType = {
    import TypeChecker._
    TypeChecker(array) match {
      case AccType(ArrayType(m, dt)) if dt.isInstanceOf[BasicType] =>
        AccType(ArrayType(n, VectorType(m /^ n, dt.asInstanceOf[BasicType])))
      case x => error(x.toString, "ArrayType")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[AccType] = {
    AsScalarAcc(fun(n), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): AccIdentifier = ???

  override def toOpenCL(opencl: ToOpenCL): VarRef = ???

  def toOpenCL(opencl: ToOpenCL, arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): VarRef = {

    val top = arrayAccess.head
    val newAAS = ((top._1 * n, top._2) :: arrayAccess.tail).map(x => (x._1, x._2 /^ n))

    ToOpenCL.acc(array, opencl, newAAS, tupleAccess)
  }

  override def prettyPrint = s"(asScalarAcc $n ${PrettyPrinter(array)})"

  override def xmlPrinter: Elem =
    <asScalarAcc n={n.toString}>
      {Core.xmlPrinter(array)}
    </asScalarAcc>
}
