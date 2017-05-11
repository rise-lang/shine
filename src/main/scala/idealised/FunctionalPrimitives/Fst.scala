package idealised.FunctionalPrimitives

import idealised.Compiling.RewriteToImperative
import idealised.Core.OperationalSemantics._
import idealised.Core._
import idealised.DSL.typed._
import idealised._

import scala.language.reflectiveCalls

import scala.xml.Elem

final case class Fst(dt1: DataType,
                     dt2: DataType,
                     record: Phrase[ExpType])
  extends ExpPrimitive {

  override lazy val `type` = exp"[$dt1]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (dt1: DataType) -> (dt2: DataType) ->
      (record :: exp"[$dt1 x $dt2]") -> `type`
  }

  override def inferTypes: Fst = {
    import TypeInference._
    val record_ = TypeInference(record)
    record_.t match {
      case ExpType(RecordType(dt1_, dt2_)) => Fst(dt1_, dt2_, record_)
      case x => error(x.toString, "ExpType(RecordType)")
    }
  }

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, record) match {
      case r: RecordData => r.fst
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Fst(fun(dt1), fun(dt2), VisitAndRebuild(record, fun))
  }

  override def prettyPrint: String = s"${PrettyPhrasePrinter(record)}._1"

  override def xmlPrinter: Elem =
    <fst dt1={ToString(dt1)} dt2={ToString(dt2)}>
      {Core.xmlPrinter(record)}
    </fst>

  override def acceptorTranslation(A: Phrase[AccType]): Phrase[CommandType] = {
    import RewriteToImperative._
    con(record)(λ(exp"[$dt1 x $dt2]")(x => A :=|dt1| Fst(dt1, dt2, x) ))
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType]): Phrase[CommandType] = {
    import RewriteToImperative._
    con(record)(λ(exp"[$dt1 x $dt2]")(x => C(Fst(dt1, dt2, x)) ))
  }
}
