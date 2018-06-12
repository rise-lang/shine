package idealised.DPIA.ImperativePrimitives

import idealised.DPIA._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics.Store
import idealised.DPIA.Types._

import scala.xml.Elem

final case class SlideAcc(n: Nat,
                          s1: Nat,
                          s2: Nat,
                          dt: DataType,
                          array: Phrase[AccType])
  extends AccPrimitive
{
  override def `type`: AccType =
    (n: Nat) -> (s1: Nat) -> (s2: Nat) -> (dt: DataType) ->
      (array :: acc"[$n.$s1.$dt]") ->
        acc"[${s2 * n + s1 - s2}.$dt]"

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[AccType] = {
    SlideAcc(f(n), f(s1), f(s2), f(dt), VisitAndRebuild(array, f))
  }

  override def eval(s: Store): OperationalSemantics.AccIdentifier = ???

  override def prettyPrint: String = s"(slideAcc ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <slideAcc n={ToString(n)} s1={ToString(s1)} s2={ToString(s2)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(array)}
    </slideAcc>
}
