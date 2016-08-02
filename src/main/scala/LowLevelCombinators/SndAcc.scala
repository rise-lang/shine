package LowLevelCombinators

import Core.OperationalSemantics._
import Core._

import scala.xml.Elem

final case class SndAcc(dt1: DataType,
                        dt2: DataType,
                        record: Phrase[AccType])
  extends LowLevelAccCombinator {

  override lazy val `type` = acc"[$dt2]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (dt1: DataType) -> (dt2: DataType) ->
      (record `:` acc"[$dt1 x $dt2]") -> `type`
  }

  override def eval(s: Store): AccIdentifier = {
    OperationalSemantics.eval(s, record) match {
      case r: RecordIdentiers => r.snd
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    SndAcc(fun(dt1), fun(dt2), VisitAndRebuild(record, fun))
  }

  override def prettyPrint: String = s"(SndAcc ${PrettyPrinter(record)})"

  override def xmlPrinter: Elem =
    <sndAcc dt1={ToString(dt1)} dt2={ToString(dt2)}>
      {Core.xmlPrinter(record)}
    </sndAcc>
}
