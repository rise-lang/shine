package idealised.LowLevelPrimitives

import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._
import idealised.Compiling.RewriteToImperative
import idealised.DSL.typed._

import scala.xml.Elem

final case class Record(dt1: DataType,
                        dt2: DataType,
                        fst: Phrase[ExpType],
                        snd: Phrase[ExpType])
  extends ExpPrimitive {

  override lazy val `type` = exp"[$dt1 x $dt2]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (dt1: DataType) -> (dt2: DataType) ->
      (fst :: exp"[$dt1]") -> (snd :: exp"[$dt2]") -> `type`
  }

  override def inferTypes: Record = {
    val fst_ = TypeInference(fst)
    val snd_ = TypeInference(snd)
    Record(fst_.t.dataType, snd_.t.dataType, fst_, snd_)
  }

  override def eval(s: Store): Data = {
    RecordData(
      OperationalSemantics.eval(s, fst),
      OperationalSemantics.eval(s, snd))
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Record(fun(dt1), fun(dt2),
      VisitAndRebuild(fst, fun), VisitAndRebuild(snd, fun))
  }

  override def prettyPrint: String =
    s"(${PrettyPhrasePrinter(fst)}, ${PrettyPhrasePrinter(snd)})"

  override def xmlPrinter: Elem =
    <record>
      <fst>
        {Core.xmlPrinter(fst)}
      </fst>
      <snd>
        {Core.xmlPrinter(snd)}
      </snd>
    </record>

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._
    acc(fst)(fstAcc(dt1, dt2, A)) `;`
      acc(snd)(sndAcc(dt1, dt2, A))
  }

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = {
    import RewriteToImperative._
    exp(fst)(λ(exp"[$dt1]")(x =>
      exp(snd)(λ(exp"[$dt2]")(y =>
        C(Record(dt1, dt2, x, y))
      ))
    ))
  }
}
