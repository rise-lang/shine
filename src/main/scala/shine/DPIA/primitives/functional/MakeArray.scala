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
final case class MakeArray(dt: DataType,
                           elements: Vector[Phrase[ExpType]]
                          ) extends ExpPrimitive with ConT {
  override val t: ExpType = expT((elements.length: Nat)`.`dt, read)

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] = {
    def rec(func: Vector[Phrase[ExpType]], imp: Vector[Phrase[ExpType]]): Phrase[CommType] = {
      func match {
        case xf +: func => con(xf)(fun(expT(dt, read))(xi =>
          rec(func, imp :+ xi)
        ))
        case _ => C(MakeArray(dt, imp))
      }
    }

    rec(elements, Vector())
  }
}
