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
final case class Pad(n: Nat,
                     l: Nat,
                     r: Nat,
                     dt: DataType,
                     padExp: Phrase[ExpType],
                     array: Phrase[ExpType]
                    ) extends ExpPrimitive with ContinuationTranslatable {
  padExp :: expT(dt, read)
  array :: expT(n `.` dt, read)
  override val t: ExpType = expT((l + n + r)`.`dt, read)

  def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                             (implicit context: TranslationContext): Phrase[CommType] =
    con(array)(λ(expT(n`.`dt, read))(x =>
      con(padExp)(λ(expT(dt, read))(p =>
        C(Pad(n, l, r, dt, p, x))))))
}
