package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class MakePair(dt1: DataType,
                          dt2: DataType,
                          access: AccessType,
                          fst: Phrase[ExpType],
                          snd: Phrase[ExpType]
                         ) extends ExpPrimitive with ContinuationTranslatable with AcceptorTranslatable {
  fst :: expT(dt1, access)
  snd :: expT(dt2, access)
  override val t: ExpType = expT(dt1 x dt2, access)

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] =
    acc(fst)(pairAcc1(dt1, dt2, A)) `;`
      acc(snd)(pairAcc2(dt1, dt2, A))

  def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                             (implicit context: TranslationContext): Phrase[CommType] =
    con(fst)(λ(expT(dt1, read))(x =>
      con(snd)(λ(expT(dt2, read))(y =>
        C(MakePair(dt1, dt2, access, x, y))))))

  override def eval(s: Store): Data = {
    PairData(
      OperationalSemantics.eval(s, fst),
      OperationalSemantics.eval(s, snd))
  }
}
