package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Idx(n: Nat,
                     dt: DataType,
                     index: Phrase[ExpType],
                     array: Phrase[ExpType]
                    ) extends ExpPrimitive {
  index :: expT(idx(n), read)
  array :: expT(n`.`dt, read)
  override val t: ExpType = expT(dt, read)
}
