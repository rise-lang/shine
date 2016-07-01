package LowLevelCombinators

import Core.OperationalSemantics._
import Core._
import apart.arithmetic.ArithExpr
import opencl.generator.OpenCLAST.VarRef

import scala.xml.Elem

case class JoinAcc(n: ArithExpr,
                   m: ArithExpr,
                   dt: DataType,
                   array: Phrase[AccType])
  extends LowLevelAccCombinator with ViewAcc with GeneratableAcc {

  override lazy val `type` = acc"[$n.$m.$dt]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    array checkType acc"[${m * n}.$dt]"
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[AccType] = {
    JoinAcc(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): AccIdentifier = ???

  override def toOpenCL(env: ToOpenCL.Environment): VarRef = ???

  override def toOpenCL(env: ToOpenCL.Environment,
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
