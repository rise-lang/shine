package shine.DPIA.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class ScatterAcc(n: Nat, m: Nat, dt: DataType,
                            indices: Phrase[ExpType],
                            array: Phrase[AccType])
  extends AccPrimitive
{

  indices :: expT(n`.`idx(m), read)
  array :: accT(n`.`dt)
  override val t: AccType = accT(m`.`dt)

  override def eval(s: Store): AccIdentifier = ???

  override def prettyPrint: String =
    s"(scatterAcc ${PrettyPhrasePrinter(indices)} ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <scatterAcc></scatterAcc>

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[AccType] =
    ScatterAcc(f.nat(n), f.nat(m), f.data(dt),
      VisitAndRebuild(indices, f), VisitAndRebuild(array, f))
}
