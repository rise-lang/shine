package AccPatterns

import Core._
import Core.OperationalSemantics._
import apart.arithmetic.ArithExpr
import ir.Type
import opencl.generator.OpenCLAST.VarRef

case class SplitAcc(array: Phrase[AccType]) extends AccPattern {

  private var n: ArithExpr = null

  override def typeCheck(): AccType = {
    import TypeChecker._
    TypeChecker(array) match {
      case AccType(ArrayType(n_, ArrayType(m, dt))) =>
        n = n_
        AccType(ArrayType(n*m, dt))
      case x => error(x.toString, "ArrayType(ArrayType)")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[AccType] = {
    val s = SplitAcc(VisitAndRebuild(array, f))
    s.n = n
    s
  }

  override def eval(s: Store): AccIdentifier = {
    ???
  }

  override def toOpenCL: VarRef = ???

  override def toOpenCL(arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): VarRef = {
    val idx = arrayAccess.head
    val stack = arrayAccess.tail

    val chunkId = idx._1 / n
    val chunkElemId = idx._1 % n

    val l = Type.getLengths( DataType.toType(t.dataType) ).reduce(_*_)

    val newAs = (chunkId, l * n) :: (chunkElemId, l) :: stack

    ToOpenCL.acc(array, newAs, tupleAccess)
  }

  override def prettyPrint: String = s"(split ${PrettyPrinter(array)})"

}
