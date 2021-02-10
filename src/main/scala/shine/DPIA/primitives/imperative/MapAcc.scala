package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.accPrimitive

@accPrimitive
final case class MapAcc(n: Nat,
                        dt1: DataType,
                        dt2: DataType,
                        f: Phrase[AccType ->: AccType],
                        array: Phrase[AccType]
                       ) extends AccPrimitive {
  f :: accT(dt1) ->: accT(dt2)
  array :: accT(n`.`dt1)
  override val t: AccType = accT(n`.`dt2)
}
