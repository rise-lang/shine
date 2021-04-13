package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class IndexAsNat(n: Nat,
                            e: Phrase[ExpType]
                           ) extends ExpPrimitive {
  e :: expT(idx(n), read)
  override val t: ExpType = expT(NatType, read)
}
