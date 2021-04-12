package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class PadEmpty(n: Nat,
                          r: Nat,
                          dt: DataType,
                          array: Phrase[ExpType]
                         ) extends ExpPrimitive {
  array :: expT(n `.` dt, write)
  override val t: ExpType = expT((n + r)`.`dt, write)
}
