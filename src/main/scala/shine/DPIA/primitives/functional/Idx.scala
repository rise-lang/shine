package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Idx(n: Nat,
                     dt: DataType,
                     index: Phrase[ExpType],
                     array: Phrase[ExpType]
                    ) extends ExpPrimitive with ConT {
  index :: expT(idx(n), read)
  array :: expT(n`.`dt, read)
  override val t: ExpType = expT(dt, read)

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] =
    con(array)(λ(expT(n`.`dt, read))(e =>
      con(index)(fun(index.t)(i =>
        C(Idx(n, dt, i, e))))))

  override def eval(s: Store): Data = {
    (OperationalSemantics.eval(s, array),
     OperationalSemantics.eval(s, index)) match {
      case (ArrayData(xs), IntData(i)) => xs(i)
      case _ => throw new Exception("This should not happen")
    }
  }
}
