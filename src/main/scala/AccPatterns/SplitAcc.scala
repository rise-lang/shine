package AccPatterns

import Core._
import Core.OperationalSemantics._
import Core.PrettyPrinter.Indent
import apart.arithmetic.ArithExpr
import ir.Type
import opencl.generator.OpenCLAST.VarRef

case class SplitAcc(n: ArithExpr,
                    m: ArithExpr,
                    dt: DataType,
                    array: Phrase[AccType]) extends AccPattern {

  override def typeCheck(): AccType = {
    import TypeChecker._
    TypeChecker(array) match {
      case AccType(ArrayType(m_, ArrayType(n_, dt_)))
        if n_ == n && m_ == m && dt == dt_ =>

        AccType(ArrayType(n*m, dt))
      case x => error(x.toString, "ArrayType(ArrayType)")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[AccType] = {
    SplitAcc(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): AccIdentifier = {
    ???
  }

  override def toOpenCL(opencl: ToOpenCL): VarRef = ???

  override def toOpenCL(opencl: ToOpenCL, arrayAccess: List[(ArithExpr, ArithExpr)], tupleAccess: List[ArithExpr]): VarRef = {
    val idx = arrayAccess.head
    val stack = arrayAccess.tail

    val chunkId = idx._1 / n
    val chunkElemId = idx._1 % n

    val l = Type.getLengths( DataType.toType(t.dataType) ).reduce(_*_)

    val newAs = (chunkId, l * n) :: (chunkElemId, l) :: stack

    ToOpenCL.acc(array, opencl, newAs, tupleAccess)
  }

  override def prettyPrint(indent: Indent): String =
    indent + s"(splitAcc\n" +
      s"${PrettyPrinter(array, indent.more)} : acc[$m.$n.$dt]\n" +
      indent + s") : acc[${n*m}.$dt]"

}
