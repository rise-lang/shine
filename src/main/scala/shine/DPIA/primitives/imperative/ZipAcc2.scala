package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.accPrimitive

@accPrimitive
final case class ZipAcc2(n: Nat,
                         dt1: DataType,
                         dt2: DataType,
                         array: Phrase[AccType]
                        ) extends AccPrimitive {
  array :: accT(n`.`(dt1 x dt2))
  override val t: AccType = accT(n`.`dt2)
}
