package AccPatterns

import Core._
import Core.OperationalSemantics._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.VarRef

case class JoinAcc(n: ArithExpr, array: Phrase[AccType]) extends AccPattern {

  override def typeCheck(): AccType = {
    import TypeChecker._
    TypeChecker(array) match {
      case AccType(ArrayType(m, dt)) =>
        AccType(ArrayType(m/n, ArrayType(n, dt)))
      case t => error(t.toString, "ArrayType")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[AccType] = {
    JoinAcc(n, VisitAndRebuild(array, f))
  }

  override def eval(s: Store): AccIdentifier = ???

  override def toOpenCL: VarRef = ???

  def toOpenCL(arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): VarRef = {

    val (firstTwo, rest) = arrayAccess.splitAt(2)

    val chunkId = firstTwo.head
    val chunkElemId = firstTwo.tail.head

    val newIdx = chunkId._1 * n + chunkElemId._1

    ToOpenCL.acc(array, (newIdx, chunkElemId._2) :: rest, tupleAccess)
  }

  override def prettyPrint: String = s"(join ${n.toString} ${PrettyPrinter(array)})"

}
