package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.accPrimitive

@accPrimitive
final case class CycleAcc(n: Nat,
                          m: Nat,
                          dt: DataType,
                          input: Phrase[AccType]
                         ) extends AccPrimitive  {
  input :: accT(m`.`dt)
  override val t: AccType = accT(n`.`dt)
}
