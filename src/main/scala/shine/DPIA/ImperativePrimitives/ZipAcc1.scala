package shine.DPIA.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class ZipAcc1(n: Nat,
                         dt1: DataType,
                         dt2: DataType,
                         array: Phrase[AccType])
  extends AccPrimitive {

  array :: accT(n`.`(dt1 x dt2))
  override val t: AccType = accT(n`.`dt1)

  override def eval(s: Store): AccIdentifier = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] =
    ZipAcc1(fun.nat(n), fun.data(dt1), fun.data(dt2), VisitAndRebuild(array, fun))

  override def xmlPrinter: Elem =
    <zipAcc1 n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      {Phrases.xmlPrinter(array)}
    </zipAcc1>

  override def prettyPrint: String = s"(ZipAcc1 ${PrettyPhrasePrinter(array)})"
}
