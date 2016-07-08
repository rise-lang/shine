package LowLevelCombinators

import Compiling.RewriteToImperative
import Core.OperationalSemantics._
import Core._
import DSL.typed._

import scala.xml.Elem

case class Snd(dt1: DataType,
               dt2: DataType,
               record: Phrase[ExpType])
  extends LowLevelExpCombinator {

  override lazy val `type` = exp"[$dt2]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    record checkType exp"[$dt1 x $dt2]"
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

  override def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[ExpType] = {
    Snd(f(dt1), f(dt2), VisitAndRebuild(record, f))
  }

  override def xmlPrinter: Elem = <snd>
    {Core.xmlPrinter(record)}
  </snd>

  override def prettyPrint: String = s"${PrettyPrinter(record)}._2"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._
    exp(this)(λ(this.t) {
      this.t.dataType match {
        case _: BasicType | _: VectorType => x => A `:=` x
        case _: ArrayType => throw new Exception("This should not happen")
        case _: RecordType => throw new Exception("This should not happen")
      }
    })
  }

  override def rewriteToImperativeExp(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = C(this)
}
