package AccPatterns

import Core._
import Core.OperationalSemantics._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.VarRef

case class JoinAcc(n: ArithExpr,
                   m: ArithExpr,
                   dt: DataType,
                   array: Phrase[AccType]) extends AccPattern {

  override def typeCheck(): AccType = {
    import TypeChecker._
    TypeChecker(array) match {
      case AccType(ArrayType(m_, dt_)) if dt_ == dt && m_ == m =>
        AccType(ArrayType(m /^ n, ArrayType(n, dt)))
      case x => error(x.toString, "ArrayType")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[AccType] = {
    JoinAcc(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): AccIdentifier = ???

  override def toOpenCL(opencl: ToOpenCL): VarRef = ???

  def toOpenCL(opencl: ToOpenCL, arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): VarRef = {

    val (firstTwo, rest) = arrayAccess.splitAt(2)

    val chunkId = firstTwo.head
    val chunkElemId = firstTwo.tail.head

    val newIdx = chunkId._1 * n + chunkElemId._1

    ToOpenCL.acc(array, opencl, (newIdx, chunkElemId._2) :: rest, tupleAccess)
  }

  override def prettyPrint: String = s"(join ${n.toString} ${PrettyPrinter(array)})"

}
