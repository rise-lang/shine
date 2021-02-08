package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.accPrimitive

@accPrimitive
final case class MapFstAcc(dt1: DataType,
                           dt2: DataType,
                           dt3: DataType,
                           f: Phrase[AccType ->: AccType],
                           record: Phrase[AccType]) extends AccPrimitive {
  f :: accT(dt3) ->: accT(dt1)
  record :: accT(dt3 x dt2)
  override val t: AccType = accT(dt1 x dt2)
}
