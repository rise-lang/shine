package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.accPrimitive

@accPrimitive
final case class UnzipAcc(n: Nat,
                          dt1: DataType,
                          dt2: DataType,
                          a: Phrase[AccType]
                         ) extends AccPrimitive {
  a :: accT((n`.`dt1) x (n`.`dt2))
  override val t: AccType = accT(n`.`(dt1 x dt2))
}
