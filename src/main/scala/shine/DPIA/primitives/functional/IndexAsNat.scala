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
final case class IndexAsNat(n: Nat,
                            e: Phrase[ExpType]
                           ) extends ExpPrimitive with ConT {
  e :: expT(idx(n), read)
  override val t: ExpType = expT(NatType, read)

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] =
    con(e)(λ(expT(idx(n), read))(x =>
      C(IndexAsNat(n, x))))
}
