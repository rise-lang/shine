package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class ToMem(dt: DataType,
                       input: Phrase[ExpType]
                      ) extends ExpPrimitive with ConT {
  input :: expT(dt, write)
  override val t: ExpType = expT(dt, read)

  def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                             (implicit context: TranslationContext): Phrase[CommType] =
    `new`(dt, tmp => acc(input)(tmp.wr) `;` C(tmp.rd))
}
