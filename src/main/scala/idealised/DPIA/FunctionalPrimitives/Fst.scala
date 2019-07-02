package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.language.reflectiveCalls
import scala.xml.Elem

final case class Fst(dt1: DataType,
                     dt2: DataType,
                     record: Phrase[ExpType]) extends ExpPrimitive
{

  override val t: ExpType =
    (dt1: DataType) -> (dt2: DataType) ->
      (record :: exp"[$dt1 x $dt2, $read]") -> exp"[$dt1, $read]"


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
      {Phrases.xmlPrinter(record)}
    </fst>

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommandType] = {
    ???
  }

  override def mapAcceptorTranslation(f: Phrase[ExpType -> ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommandType] =
    ???

  override def continuationTranslation(C: Phrase[ExpType -> CommandType])
                                      (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._
    con(record)(λ(exp"[$dt1 x $dt2, $read]")(x => C(Fst(dt1, dt2, x)) ))
  }
}
