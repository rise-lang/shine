package idealised.ImperativePrimitives

import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._

import scala.xml.Elem

final case class RecordAcc2(dt1: DataType,
                            dt2: DataType,
                            record: Phrase[AccType])
  extends AccPrimitive {

  override lazy val `type` = acc"[$dt2]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (dt1: DataType) -> (dt2: DataType) ->
      (record :: acc"[$dt1 x $dt2]") -> `type`
  }

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
      {Core.xmlPrinter(record)}
    </recordAcc2>

  override def prettyPrint: String = s"(RecordAcc2 ${PrettyPhrasePrinter(record)})"
}
