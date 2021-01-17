package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class DepZip(n: Nat,
                        ft1: NatToData,
                        ft2: NatToData,
                        e1: Phrase[ExpType],
                        e2: Phrase[ExpType]
                       ) extends ExpPrimitive with ContinuationTranslatable {
  e1 :: expT(n`.d`ft1, read)
  e2 :: expT(n`.d`ft2, read)
  override val t: ExpType = expT(n`.d`{ i => PairType(ft1(i), ft2(i)) }, read)

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] =
    con(e1)(λ(ExpType(DepArrayType(n, ft1), read))(x =>
      con(e2)(λ(ExpType(DepArrayType(n, ft2), read))(y =>
        C(DepZip(n, ft1, ft2, x, y)) )) ))
}
