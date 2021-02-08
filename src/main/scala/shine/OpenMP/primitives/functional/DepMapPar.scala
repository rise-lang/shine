package shine.OpenMP.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenMP.primitives.intermediate.DepMapParI
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class DepMapPar(n: Nat,
                           ft1: NatToData,
                           ft2: NatToData,
                           f: Phrase[`(nat)->:`[ExpType ->: ExpType]],
                           array: Phrase[ExpType]
                          ) extends ExpPrimitive with ConT with AccT {
  f :: f.t.x ->: expT(ft1(f.t.x), read) ->: expT(ft2(f.t.x), write)
  array :: expT(n `.d` ft1, read)
  override val t: ExpType = expT(n`.d`ft2, write)

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] =
    con(array)(λ(expT(n`.d`ft1, read))(x =>
      DepMapParI(n, ft1, ft2, _Λ_[NatKind]()((k: NatIdentifier) =>
        λ(expT(ft1(k), read))(x => λ(accT(ft2(k)))(o => {
          acc(f(k)(x))(o)
        }))), x, A)))

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] =
    `new`(n`.d`ft2, λ(varT(n`.d`ft2))(tmp =>
      acc(this)(tmp.wr) `;` C(tmp.rd) ))
}
