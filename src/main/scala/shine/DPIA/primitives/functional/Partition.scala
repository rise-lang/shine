package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Partition(n: Nat,
                           m: Nat,
                           lenF: NatToNat,
                           dt: DataType,
                           array: Phrase[ExpType]
                          ) extends ExpPrimitive {
  array :: expT(n`.`dt, read)
  override val t: ExpType = expT(m`.d`{ i => lenF(i)`.`dt }, read)
}
