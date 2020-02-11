package shine.DPIA.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class UnzipAcc(n: Nat,
                          dt1: DataType,
                          dt2: DataType,
                          a: Phrase[AccType])
  extends AccPrimitive
{

  a :: accT((n`.`dt1) x (n`.`dt2))
  override val t: AccType = accT(n`.`(dt1 x dt2))

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    UnzipAcc(fun.nat(n), fun.data(dt1), fun.data(dt2), VisitAndRebuild(a, fun))
  }

  override def eval(s: Store): AccIdentifier = ???


  override def prettyPrint: String =
    s"(unzipAcc ${PrettyPhrasePrinter(a)})"

  override def xmlPrinter: Elem =
    <joinAcc n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      {Phrases.xmlPrinter(a)}
    </joinAcc>
}
