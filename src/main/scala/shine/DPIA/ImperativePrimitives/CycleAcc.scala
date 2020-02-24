package shine.DPIA.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.{AccIdentifier, Store}
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

final case class CycleAcc(n: Nat,
                          m: Nat,
                          dt: DataType,
                          input: Phrase[AccType])
  extends AccPrimitive
{
  input :: accT(m`.`dt)
  override val t: AccType = accT(n`.`dt)

  override def eval(s: Store): AccIdentifier = ???

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[AccType] =
    CycleAcc(v.nat(n), v.nat(m), v.data(dt), VisitAndRebuild(input, v))

  override def xmlPrinter: xml.Elem =
    <cycleAcc n={ToString(n)} m={ToString(m)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(input)}
    </cycleAcc>

  override def prettyPrint: String = s"(cycleAcc $input)"
}
