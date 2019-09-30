package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class ReorderAcc(n: Nat,
                            dt: DataType,
                            idxF: Phrase[ExpType ->: ExpType],
                            array: Phrase[AccType])
  extends AccPrimitive
{
  override val t: AccType =
    (n: Nat) ->: (dt: DataType) ->:
      (idxF :: t"exp[idx($n), $read] -> exp[idx($n), $read]") ->:
        (array :: acc"[$n.$dt]") ->:
          acc"[$n.$dt]"

  override def eval(s: Store): AccIdentifier = ???

  override def prettyPrint: String =
    s"(reorderAcc ${PrettyPhrasePrinter(idxF)} ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <reorderAcc n={ToString(n)} dt={ToString(dt)}>
      <idxF>{Phrases.xmlPrinter(idxF)}</idxF>
      <input>{Phrases.xmlPrinter(array)}</input>
    </reorderAcc>

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[AccType] =
    ReorderAcc(f.nat(n), f.data(dt), VisitAndRebuild(idxF, f), VisitAndRebuild(array, f))
}
