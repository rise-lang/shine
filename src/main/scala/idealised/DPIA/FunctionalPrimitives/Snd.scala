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

final case class Snd(dt1: DataType,
                     dt2: DataType,
                     record: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType =
    (dt1: DataType) ->: (dt2: DataType) ->:
      (record :: exp"[$dt1 x $dt2]") ->: exp"[$dt2]"

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, record) match {
      case r: RecordData => r.snd
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Snd(f.data(dt1), f.data(dt2), VisitAndRebuild(record, f))
  }

  override def xmlPrinter: Elem = <snd>
    {Phrases.xmlPrinter(record)}
  </snd>

  override def prettyPrint: String = s"${PrettyPhrasePrinter(record)}._2"

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(record)(λ(exp"[$dt1 x $dt2]")(x => A :=|dt2| Snd(dt1, dt2, x) ))
  }

  override def mapAcceptorTranslation(f: Phrase[ExpType ->: ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommType] =
    ???

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(record)(λ(exp"[$dt1 x $dt2]")(x => C(Snd(dt1, dt2, x)) ))
  }
}
