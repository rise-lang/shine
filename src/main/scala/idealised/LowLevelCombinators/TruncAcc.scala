package idealised.LowLevelCombinators

import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._

import scala.xml.Elem

final case class TruncAcc(n: Nat,
                          m: Nat,
                          dt: DataType,
                          array: Phrase[AccType])
  extends LowLevelAccCombinator {

  override lazy val `type` = acc"[$m.$dt]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (n: Nat) -> (m: Nat) -> (dt: DataType) ->
      (array `:` acc"[$n.$dt]") -> `type`
  }

  override def eval(s: Store): AccIdentifier = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    TruncAcc(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def prettyPrint: String = s"(truncAcc ${PrettyPrinter(array)})"

  override def xmlPrinter: Elem =
    <truncAcc n={ToString(n)} m={ToString(m)} dt={ToString(dt)}>
      {Core.xmlPrinter(array)}
    </truncAcc>
}
