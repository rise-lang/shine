package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.accPrimitive

@accPrimitive
final case class AsScalarAcc(n: Nat,
                             m: Nat,
                             dt: DataType,
                             array: Phrase[AccType]
                            )extends AccPrimitive {
  array :: accT((m * n)`.`dt)
  override val t: AccType = accT(n`.`vec(m, dt))
}
