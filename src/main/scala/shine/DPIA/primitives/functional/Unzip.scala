package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Unzip(n: Nat,
                       dt1: DataType,
                       dt2: DataType,
                       access: AccessType,
                       e: Phrase[ExpType]
                      ) extends ExpPrimitive {
  e :: expT(n`.`(dt1 x dt2), access)
  override val t: ExpType = expT((n`.`dt1) x (n`.`dt2), access)
}
