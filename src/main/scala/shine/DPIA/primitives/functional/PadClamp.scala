package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

// TODO: invalid for empty array
@expPrimitive
final case class PadClamp(n: Nat,
                          l: Nat,
                          r: Nat,
                          dt: DataType,
                          array: Phrase[ExpType]
                         ) extends ExpPrimitive {
  array :: expT(n `.` dt, read)
  override val t: ExpType = expT((l + n + r)`.`dt, read)
}
