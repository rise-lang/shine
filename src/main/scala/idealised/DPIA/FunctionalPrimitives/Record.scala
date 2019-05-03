package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class Record(dt1: DataType,
                        dt2: DataType,
                        fst: Phrase[ExpType],
                        snd: Phrase[ExpType])
  extends ExpPrimitive {

  override val t: ExpType =
    (dt1: DataType) -> (dt2: DataType) ->
      (fst :: exp"[$dt1]") -> (snd :: exp"[$dt2]") -> exp"[$dt1 x $dt2]"

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
        {Phrases.xmlPrinter(fst)}
      </fst>
      <snd>
        {Phrases.xmlPrinter(snd)}
      </snd>
    </record>

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    acc(fst)(recordAcc1(dt1, dt2, A)) `;`
      acc(snd)(recordAcc2(dt1, dt2, A))
  }

  override def mapAcceptorTranslation(f: Phrase[ExpType -> ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommandType] =
    ???

  override def continuationTranslation(C: Phrase[->[ExpType, CommandType]])
                                      (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    con(fst)(λ(exp"[$dt1]")(x =>
      con(snd)(λ(exp"[$dt2]")(y =>
        C(Record(dt1, dt2, x, y)) )) ))
  }
}
