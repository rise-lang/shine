package shine.DPIA.primitives.imperative

import arithexpr.arithmetic.BigSum
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.accPrimitive

@accPrimitive
final case class DepJoinAcc(n: Nat,
                            lenF:NatToNat,
                            dt: DataType,
                            array: Phrase[AccType]
                           ) extends AccPrimitive {
  array :: accT(BigSum(from=0, upTo = n-1, i => lenF(i))`.`dt)
  override val t: AccType = accT(n`.d`{ i => ArrayType(lenF(i), dt) })
}
