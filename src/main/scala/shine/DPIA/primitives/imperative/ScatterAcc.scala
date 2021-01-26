package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.accPrimitive

@accPrimitive
final case class ScatterAcc(n: Nat, m: Nat, dt: DataType,
                            indices: Phrase[ExpType],
                            array: Phrase[AccType]) extends AccPrimitive {
  indices :: expT(n`.`idx(m), read)
  array :: accT(n`.`dt)
  override val t: AccType = accT(m`.`dt)
}
