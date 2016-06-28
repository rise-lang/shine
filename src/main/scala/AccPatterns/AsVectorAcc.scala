package AccPatterns

import Core.OperationalSemantics._
import Core._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.VarRef

import scala.xml.Elem

case class AsVectorAcc(array: Phrase[AccType]) extends AccPattern {

  private var n: ArithExpr = null

  override def typeCheck(): AccType = {
    import TypeChecker._
    TypeChecker(array) match {
      case AccType(ArrayType(n_, VectorType(m, dt))) =>
        n = n_
        AccType(ArrayType(n * m, dt))
      case x => error(x.toString, "ArrayType(VectorType)")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[AccType] = {
    val s = AsVectorAcc(VisitAndRebuild(array, fun))
    s.n = fun(n)
    s
  }

  override def eval(s: Store): AccIdentifier = ???

  override def toOpenCL(opencl: ToOpenCL): VarRef = ???

  override def toOpenCL(opencl: ToOpenCL,
                        arrayAccess: List[(ArithExpr, ArithExpr)],
                        tupleAccess: List[ArithExpr],
                        dt: DataType): VarRef = {
    val top = arrayAccess.head
    val newAAS = ((top._1 /^ n, top._2) :: arrayAccess.tail).map(x => (x._1, x._2 * n))

    ToOpenCL.acc(array, opencl, newAAS, tupleAccess, dt)
  }

  override def prettyPrint: String = s"(asVectorAcc ${PrettyPrinter(array)})"

  override def xmlPrinter: Elem =
    <asVectorAcc n={ToString(n)}>
      {Core.xmlPrinter(array)}
    </asVectorAcc>
}
