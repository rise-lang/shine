package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

// TODO: invalid for empty array
@expPrimitive
final case class PadClamp(n: Nat,
                          l: Nat,
                          r: Nat,
                          dt: DataType,
                          array: Phrase[ExpType]
                         ) extends ExpPrimitive with ContinuationTranslatable {
  array :: expT(n `.` dt, read)
  override val t: ExpType = expT((l + n + r)`.`dt, read)

  def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                             (implicit context: TranslationContext): Phrase[CommType] =
    con(array)(λ(expT(n`.`dt, read))(x =>
      C(PadClamp(n, l, r, dt, x))))
}
