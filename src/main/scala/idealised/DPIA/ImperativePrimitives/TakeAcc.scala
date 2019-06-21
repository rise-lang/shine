package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class TakeAcc(n: Nat,
                         m: Nat,
                         dt: DataType,
                         array: Phrase[AccType])
  extends AccPrimitive {

  override val t: AccType =
    (n: Nat) -> (m: Nat) -> (dt: DataType) ->
      (array :: acc"[${n + m}.$dt]") -> acc"[$n.$dt]"

  override def eval(s: Store): AccIdentifier = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    TakeAcc(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def prettyPrint: String = s"(takeAcc ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <takeAcc n={ToString(n)} m={ToString(m)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(array)}
    </takeAcc>
}
