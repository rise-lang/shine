package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Fst(dt1: DataType,
                     dt2: DataType,
                     pair: Phrase[ExpType]
                    ) extends ExpPrimitive with ContinuationTranslatable {
  pair :: expT(dt1 x dt2, read)
  override val t: ExpType = expT(dt1, read)

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] =
    con(pair)(λ(expT(dt1 x dt2, read))(x =>
      C(Fst(dt1, dt2, x))))

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, pair) match {
      case r: PairData => r.fst
      case _ => throw new Exception("This should not happen")
    }
  }
}
