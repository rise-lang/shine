package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._

import scala.xml.Elem

final case class Pair(
  dt1: DataType,
  dt2: DataType,
  access: AccessType,
  fst: Phrase[ExpType],
  snd: Phrase[ExpType]
) extends ExpPrimitive {

  fst :: expT(dt1, access)
  snd :: expT(dt2, access)
  override val t: ExpType = expT(dt1 x dt2, access)

  override def eval(s: Store): Data = {
    PairData(
      OperationalSemantics.eval(s, fst),
      OperationalSemantics.eval(s, snd))
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[ExpType] = {
    Pair(fun.data(dt1), fun.data(dt2), fun.access(access),
      VisitAndRebuild(fst, fun), VisitAndRebuild(snd, fun))
  }

  override def prettyPrint: String =
    s"(${PrettyPhrasePrinter(fst)}, ${PrettyPhrasePrinter(snd)})"

  override def xmlPrinter: Elem =
    <record access={ToString(access)}>
      <fst>
        {Phrases.xmlPrinter(fst)}
      </fst>
      <snd>
        {Phrases.xmlPrinter(snd)}
      </snd>
    </record>

  override def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import TranslationToImperative._

    acc(fst)(pairAcc1(dt1, dt2, A)) `;`
      acc(snd)(pairAcc2(dt1, dt2, A))
  }

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import TranslationToImperative._

    con(fst)(λ(expT(dt1, read))(x =>
      con(snd)(λ(expT(dt2, read))(y =>
        C(Pair(dt1, dt2, access, x, y)) )) ))
  }
}
