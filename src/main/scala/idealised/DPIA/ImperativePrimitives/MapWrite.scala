package idealised.DPIA.ImperativePrimitives

import idealised.DPIA._
import idealised.DPIA.DSL._
import idealised.DPIA.Types._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._

import scala.language.reflectiveCalls

final case class MapWrite(n: Nat,
                          dt1: DataType,
                          dt2: DataType,
                          f: Phrase[AccType -> ((AccType -> CommandType) -> CommandType)],
                          output: Phrase[AccType])
  extends AccPrimitive
{
  override val `type`: AccType =
    (n: Nat) -> (dt1: DataType) -> (dt2: DataType) ->
      (f :: acc"[$dt2]" -> (t"acc[$dt1] -> comm" -> comm)) ->
      (output :: acc"[$n.$dt2]") -> acc"[$n.$dt1]"

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[AccType] = {
    MapWrite(v(n), v(dt1), v(dt2), VisitAndRebuild(f, v), VisitAndRebuild(output, v))
  }

  override def eval(s: Store): AccIdentifier = ???

  override def prettyPrint: String = s"(mapWrite $f $output)"

  override def xmlPrinter: xml.Elem =
    <mapWrite n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <f>
        {Phrases.xmlPrinter(f)}
      </f>
      <output>
        {Phrases.xmlPrinter(output)}
      </output>
    </mapWrite>
}
