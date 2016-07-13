package LowLevelCombinators

import Core.OperationalSemantics._
import Core._

import scala.xml.Elem

final case class FstAcc(dt1: DataType,
                        dt2: DataType,
                        record: Phrase[AccType])
  extends LowLevelAccCombinator {

  override lazy val `type` = acc"[$dt1]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (dt1: DataType) -> (dt2: DataType) ->
      (record `:` acc"[$dt1 x $dt2]") -> `type`
  }

  override def eval(s: Store): AccIdentifier = {
    OperationalSemantics.eval(s, record) match {
      case r: RecordIdentiers => r.fst
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[AccType] =
    FstAcc(fun(dt1), fun(dt2), VisitAndRebuild(record, fun))


  override def xmlPrinter: Elem =
    <fstAcc dt1={ToString(dt1)} dt2={ToString(dt2)}>
      {Core.xmlPrinter(record)}
    </fstAcc>

  override def prettyPrint: String = s"(FstAcc ${PrettyPrinter(record)})"
}
