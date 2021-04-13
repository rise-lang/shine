package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.accPrimitive

@accPrimitive
final case class IdxAcc(n: Nat,
                        dt: DataType,
                        index: Phrase[ExpType],
                        array: Phrase[AccType]
                       )extends AccPrimitive {
  index :: expT(idx(n), read)
  array :: accT(n`.`dt)
  override val t: AccType = accT(dt)
}
