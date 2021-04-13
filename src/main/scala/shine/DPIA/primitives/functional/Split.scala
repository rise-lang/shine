package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Split(n: Nat,
                       m: Nat,
                       w: AccessType,
                       dt: DataType,
                       array: Phrase[ExpType]
                      ) extends ExpPrimitive {
  array :: expT((m * n)`.`dt, w)
  override val t: ExpType = expT(m`.`(n`.`dt), w)
}
