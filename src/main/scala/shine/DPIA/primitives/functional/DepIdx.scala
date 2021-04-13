package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class DepIdx(n: Nat,
                        ft: NatToData,
                        index: Nat,
                        array: Phrase[ExpType]
                       ) extends ExpPrimitive {
  array :: expT(n`.d`ft, read)
  override val t: ExpType = expT(ft(index), read)
}
