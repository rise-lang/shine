package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Cast(dt1: BasicType,
                      dt2: BasicType,
                      e: Phrase[ExpType]
                     )extends ExpPrimitive with ContinuationTranslatable {
  e :: expT(dt1, read)
  override val t: ExpType = expT(dt2, read)

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] =
    con(e)(fun(e.t)(x =>
      C(Cast(dt1, dt2, x))))
}
