package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Compilation.CodeGenerator
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class RecordAcc1(dt1: DataType,
                            dt2: DataType,
                            record: Phrase[AccType])
  extends AccPrimitive {

  override val t: AccType =
    (dt1: DataType) -> (dt2: DataType) ->
      (record :: acc"[$dt1 x $dt2]") ->
        acc"[$dt1]"

  override def eval(s: Store): AccIdentifier = {
    OperationalSemantics.eval(s, record) match {
      case r: RecordIdentifier => r.fst
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] =
    RecordAcc1(fun(dt1), fun(dt2), VisitAndRebuild(record, fun))


  override def xmlPrinter: Elem =
    <recordAcc1 dt1={ToString(dt1)} dt2={ToString(dt2)}>
      {Phrases.xmlPrinter(record)}
    </recordAcc1>

  override def prettyPrint: String = s"(RecordAcc1 ${PrettyPhrasePrinter(record)})"
}
