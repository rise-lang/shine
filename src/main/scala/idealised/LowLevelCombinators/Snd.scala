package idealised.LowLevelCombinators

import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._
import idealised.Compiling.RewriteToImperative
import idealised.DSL.typed._
import idealised.MidLevelCombinators.MapI

import scala.xml.Elem

final case class Snd(dt1: DataType,
                     dt2: DataType,
                     record: Phrase[ExpType])
  extends LowLevelExpCombinator {

  override lazy val `type` = exp"[$dt2]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (dt1: DataType) -> (dt2: DataType) ->
      (record :: exp"[$dt1 x $dt2]") -> `type`
  }

  override def inferTypes: Snd = {
    import TypeInference._
    val record_ = TypeInference(record)
    record_.t match {
      case ExpType(RecordType(dt1_, dt2_)) => Snd(dt1_, dt2_, record_)
      case x => error(x.toString, "ExpType(RecordType)")
    }
  }

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, record) match {
      case r: RecordData => r.snd
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Snd(f(dt1), f(dt2), VisitAndRebuild(record, f))
  }

  override def xmlPrinter: Elem = <snd>
    {Core.xmlPrinter(record)}
  </snd>

  override def prettyPrint: String = s"${PrettyPrinter(record)}._2"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._
    exp(record)(λ(exp"[$dt1 x $dt2]")(e =>
      dt2 match {
        case b: BasicType => A `:=` Snd(dt1, dt2, e)
        case ArrayType(n, dt) =>
          MapI(n, dt, dt, A, λ(AccType(dt))(a => λ(ExpType(dt))(e => acc(e)(a))), Snd(dt1, dt2, e))
        case RecordType(dt11, dt12) =>
          acc(fst(Snd(dt1, dt2, e)))(fstAcc(dt11, dt12, A)) `;`
            acc(snd(Snd(dt1, dt2, e)))(sndAcc(dt11, dt12, A))
        case _: DataTypeIdentifier => throw new Exception("This should not happen")
      }
    ))
  }

  override def rewriteToImperativeExp(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] =
    RewriteToImperative.exp(record)(λ(exp"[$dt1 x $dt2]")(e =>
      C(Snd(dt1, dt2, e))
    ))
}
