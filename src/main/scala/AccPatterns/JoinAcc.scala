package AccPatterns

import Core._
import Core.OperationalSemantics._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.VarRef

import scala.xml.Elem

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

  override def toOpenCL(env: ToOpenCL.Environment): VarRef = ???

  def toOpenCL(env: ToOpenCL.Environment,
               arrayAccess: List[(ArithExpr, ArithExpr)],
               tupleAccess: List[ArithExpr],
               dt: DataType): VarRef = {

    val (firstTwo, rest) = arrayAccess.splitAt(2)

    val chunkId = firstTwo.head
    val chunkElemId = firstTwo.tail.head

    val newIdx = chunkId._1 * m + chunkElemId._1

    ToOpenCL.acc(array, env, (newIdx, chunkElemId._2) :: rest, tupleAccess, dt)
  }

  override def prettyPrint: String =
    s"(joinAcc ${PrettyPrinter(array)})"

  override def xmlPrinter: Elem =
    <joinAcc n={ToString(n)} m={ToString(m)} dt={ToString(dt)}>
      {Core.xmlPrinter(array)}
    </joinAcc>
}
