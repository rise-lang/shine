package shine.DPIA.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

// this drops n many elements from an array of m elements
final case class DropAcc(n: Nat,
                         m: Nat,
                         dt: DataType,
                         array: Phrase[AccType])
  extends AccPrimitive {

  array :: accT({n + m}`.`dt)
  override val t: AccType = accT({m - n}`.`dt)

  override def eval(s: Store): AccIdentifier = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    DropAcc(fun.nat(n), fun.nat(m), fun.data(dt), VisitAndRebuild(array, fun))
  }

  override def prettyPrint: String = s"(dropAcc ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <dropAcc n={ToString(n)} m={ToString(m)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(array)}
    </dropAcc>
}
