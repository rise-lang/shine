package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class MakeArray(dt: DataType,
                           elements: Vector[Phrase[ExpType]]
                          ) extends ExpPrimitive {
  override val t: ExpType = expT((elements.length: Nat)`.`dt, read)
}
