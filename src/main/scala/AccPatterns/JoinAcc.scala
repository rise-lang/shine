package AccPatterns

import Core._
import Core.OperationalSemantics._
import Core.PrettyPrinter.Indent
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.VarRef

case class JoinAcc(n: ArithExpr,
                   m: ArithExpr,
                   dt: DataType,
                   array: Phrase[AccType]) extends AccPattern {

  override def typeCheck(): AccType = {
    import TypeChecker._
    TypeChecker(array) match {
      case AccType(ArrayType(mn_, dt_)) =>
        if (dt_ == dt && mn_ == (m * n)) {
          AccType(ArrayType(n, ArrayType(m, dt)))
        } else {
          error(s"[$mn_.$dt_] -> [$n.${mn_ /^ n}.$dt_]", s"[${m*n}.$dt] -> [$n.$m.$dt]")
        }
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

  override def prettyPrint(indent: Indent): String =
    indent + s"(joinAcc\n" +
      indent.more + s"(${PrettyPrinter(array, indent.more)}) : acc[${n*m}.$dt]\n" +
      indent + s") : acc[$n.$m.$dt]\n"

}
