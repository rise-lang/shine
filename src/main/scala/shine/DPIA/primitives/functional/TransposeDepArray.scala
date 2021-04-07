package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class TransposeDepArray(n: Nat,
                                   m: Nat,
                                   f: NatToData,
                                   array: Phrase[ExpType]
                                  ) extends ExpPrimitive {
  array :: expT(n`.`(m`.d`f), read)
  override val t: ExpType = expT(m`.d`{ k => n`.`f(k) }, read)
}
