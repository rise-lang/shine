package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class MapAcc(n: Nat,
                        dt1: DataType,
                        dt2: DataType,
                        f: Phrase[AccType -> AccType],
                        array: Phrase[AccType])
  extends AccPrimitive {

  override val t: AccType =
    (n: Nat) -> (dt1: DataType) -> (dt2: DataType) ->
      (f :: t"acc[$dt1] -> acc[$dt2]") ->
      (array :: acc"[$n.$dt1]") -> acc"[$n.$dt2]"

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    MapAcc(fun(n), fun(dt1), fun(dt2), VisitAndRebuild(f, fun), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): AccIdentifier = ???

  override def prettyPrint: String = s"(mapAcc ${PrettyPhrasePrinter(f)} ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <mapAcc n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <f type={ToString(AccType(dt1) -> AccType(dt2))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <input type={ToString(AccType(ArrayType(n, dt1)))}>
        {Phrases.xmlPrinter(array)}
      </input>
    </mapAcc>
}
