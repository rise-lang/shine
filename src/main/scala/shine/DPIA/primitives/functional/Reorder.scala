package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Reorder(n: Nat,
                         dt: DataType,
                         access: AccessType,
                         idxF: NatToNat,
                         idxFinv: NatToNat,
                         input: Phrase[ExpType]
                        ) extends ExpPrimitive {
  input :: expT(n`.`dt, access)
  override val t: ExpType = expT(n`.`dt, access)
}
