package shine.DPIA.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class ReorderAcc(n: Nat,
                            dt: DataType,
                            idxF: Phrase[ExpType ->: ExpType],
                            array: Phrase[AccType])
  extends AccPrimitive
{

  idxF :: expT(idx(n), read) ->: expT(idx(n), read)
  array :: accT(n`.`dt)
  override val t: AccType = accT(n`.`dt)

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
