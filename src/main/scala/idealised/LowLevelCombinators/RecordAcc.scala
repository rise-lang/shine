package idealised.LowLevelCombinators

import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._

import scala.xml.Elem

final case class RecordAcc(dt1: DataType,
                           dt2: DataType,
                           fst: Phrase[AccType],
                           snd: Phrase[AccType])
  extends LowLevelAccCombinator {

  override lazy val `type` = acc"[$dt1 x $dt2]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (dt1: DataType) -> (dt2: DataType) ->
      (fst `:` acc"[$dt1]") -> (snd `:` acc"[$dt2]") -> `type`
  }

  override def eval(s: Store): AccIdentifier = {
    RecordIdentiers(
      OperationalSemantics.eval(s, fst),
      OperationalSemantics.eval(s, snd))
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    RecordAcc(fun(dt1), fun(dt2),
      VisitAndRebuild(fst, fun), VisitAndRebuild(snd, fun))
  }

  override def prettyPrint: String =
    s"(${PrettyPrinter(fst)}, ${PrettyPrinter(snd)})"

  override def xmlPrinter: Elem =
    <recordAcc>
      <fst>
        {Core.xmlPrinter(fst)}
      </fst>
      <snd>
        {Core.xmlPrinter(snd)}
      </snd>
    </recordAcc>
}
