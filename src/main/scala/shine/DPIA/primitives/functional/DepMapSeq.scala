package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.intermediate.DepMapSeqI
import shine.macros.Primitive.expPrimitive

//noinspection TypeAnnotation
@expPrimitive
final case class DepMapSeq(unroll: Boolean)
                          (val n: Nat,
                           val ft1: NatToData,
                           val ft2: NatToData,
                           val f: Phrase[`(nat)->:`[ExpType ->: ExpType]],
                           val array: Phrase[ExpType]
                          ) extends ExpPrimitive with AccT {
  f :: f.t.x ->: expT(ft1(f.t.x), read) ->: expT(ft2(f.t.x), write)
  array :: expT(n `.d` ft1, read)
  override val t: ExpType = expT(n`.d`ft2, write)

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] =
    con(array)(λ(expT(n`.d`ft1, read))(x =>
      DepMapSeqI(unroll)(n, ft1, ft2, _Λ_[NatKind]()((k: NatIdentifier) =>
        λ(expT(ft1(k), read))(x => λ(accT(ft2(k)))(o => {
          acc(f(k)(x))(o)
        }))), x, A)))
}
