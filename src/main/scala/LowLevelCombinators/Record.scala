package LowLevelCombinators

import Core.OperationalSemantics._
import Core._

import scala.xml.Elem

final case class Record(dt1: DataType,
                        dt2: DataType,
                        fst: Phrase[ExpType],
                        snd: Phrase[ExpType])
  extends LowLevelExpCombinator {

  override lazy val `type` = exp"[$dt1 x $dt2]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (dt1: DataType) -> (dt2: DataType) ->
      (fst `:` exp"[$dt1]") -> (snd `:` exp"[$dt2]") -> `type`
  }

  override def inferTypes: Record = {
    import TypeInference._
    val fst_ = TypeInference(fst)
    val snd_ = TypeInference(snd)
    Record(fst_.t.dataType, snd_.t.dataType, fst_, snd_)
  }

  override def eval(s: Store): Data = {
    RecordData(
      OperationalSemantics.eval(s, fst),
      OperationalSemantics.eval(s, snd))
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[ExpType] = {
    Record(fun(dt1), fun(dt2),
      VisitAndRebuild(fst, fun), VisitAndRebuild(snd, fun))
  }

  override def prettyPrint: String =
    s"(${PrettyPrinter(fst)}, ${PrettyPrinter(snd)})"

  override def xmlPrinter: Elem =
    <record>
      <fst>
        {Core.xmlPrinter(fst)}
      </fst>
      <snd>
        {Core.xmlPrinter(snd)}
      </snd>
    </record>

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = ???

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = ???
}
