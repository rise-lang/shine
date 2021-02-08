package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.accPrimitive

@accPrimitive
final case class DepIdxAcc(n: Nat,
                           ft:NatToData,
                           index: Nat,
                           array: Phrase[AccType]
                          ) extends AccPrimitive {
  array :: accT(n`.d`ft)
  override val t: AccType = accT(ft(index))
}
