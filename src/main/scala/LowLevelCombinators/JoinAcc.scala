package LowLevelCombinators

import Core.OperationalSemantics._
import Core._
import apart.arithmetic.ArithExpr

import scala.xml.Elem

case class JoinAcc(n: ArithExpr,
                   m: ArithExpr,
                   dt: DataType,
                   array: Phrase[AccType])
  extends LowLevelAccCombinator {

  override lazy val `type` = acc"[$n.$m.$dt]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    array checkType acc"[${m * n}.$dt]"
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[AccType] = {
    JoinAcc(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): AccIdentifier = ???


  override def prettyPrint: String =
    s"(joinAcc ${PrettyPrinter(array)})"

  override def xmlPrinter: Elem =
    <joinAcc n={ToString(n)} m={ToString(m)} dt={ToString(dt)}>
      {Core.xmlPrinter(array)}
    </joinAcc>
}
