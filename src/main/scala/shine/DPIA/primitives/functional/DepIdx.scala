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
final case class DepIdx(n: Nat,
                        ft: NatToData,
                        index: Nat,
                        array: Phrase[ExpType]
                       ) extends ExpPrimitive with AccT {
  array :: expT(n`.d`ft, read)
  override val t: ExpType = expT(ft(index), read)

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] =
    con(array)(λ(expT(n`.d`ft, read))(x =>
      A :=| ft(index) | DepIdx(n, ft, index, x)))
}
