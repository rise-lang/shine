package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class UnzipAcc(n: Nat,
                          dt1: DataType,
                          dt2: DataType,
                          a: Phrase[AccType])
  extends AccPrimitive
{

  override val t: AccType =
    (n: Nat) -> (dt1: DataType) -> (dt2: DataType) ->
      (a :: AccType(RecordType(ArrayType(n, dt1), ArrayType(n, dt2)))) ->
        acc"[$n.($dt1 x $dt2)]"


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
