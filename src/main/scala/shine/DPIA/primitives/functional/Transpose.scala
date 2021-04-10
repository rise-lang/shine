package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Transpose(n: Nat,
                           m: Nat,
                           dt: DataType,
                           access: AccessType,
                           array: Phrase[ExpType]
                          ) extends ExpPrimitive {
  array :: expT(n`.`(m`.`dt), access)
  override val t: ExpType = expT(m`.`(n`.`dt), access)
}
