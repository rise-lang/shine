package AccPatterns

import Core._
import Core.OperationalSemantics._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.VarRef

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

  override def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[AccType] = {
    AsScalarAcc(n, VisitAndRebuild(array, f))
  }

  override def eval(s: Store): AccIdentifier = ???

  override def toOpenCL(opencl: ToOpenCL): VarRef = ???

  def toOpenCL(opencl: ToOpenCL, arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): VarRef = {

    val top = arrayAccess.head
    val newAAS = ((top._1 * n, top._2) :: arrayAccess.tail).map(x => (x._1, x._2 /^ n))

    ToOpenCL.acc(array, opencl, newAAS, tupleAccess)
  }

  override def prettyPrint: String = s"(asScalarAcc ${n.toString} ${PrettyPrinter(array)})"

}
