package shine.DPIA.primitives.functional

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
                    ) extends ExpPrimitive {
  padExp :: expT(dt, read)
  array :: expT(n `.` dt, read)
  override val t: ExpType = expT((l + n + r)`.`dt, read)
}
