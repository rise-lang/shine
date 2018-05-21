package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Compilation.CodeGenerator
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class RecordAcc2(dt1: DataType,
                            dt2: DataType,
                            record: Phrase[AccType])
  extends AccPrimitive {

  override val `type`: AccType =
    (dt1: DataType) -> (dt2: DataType) ->
      (record :: acc"[$dt1 x $dt2]") ->
        acc"[$dt2]"

  override def eval(s: Store): AccIdentifier = {
    OperationalSemantics.eval(s, record) match {
      case r: RecordIdentifier => r.snd
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    RecordAcc2(fun(dt1), fun(dt2), VisitAndRebuild(record, fun))
  }

  override def xmlPrinter: Elem =
    <recordAcc2 dt1={ToString(dt1)} dt2={ToString(dt2)}>
      {Phrases.xmlPrinter(record)}
    </recordAcc2>

  override def prettyPrint: String = s"(RecordAcc2 ${PrettyPhrasePrinter(record)})"
}
