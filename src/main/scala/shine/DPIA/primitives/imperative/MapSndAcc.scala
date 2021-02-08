package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.accPrimitive

@accPrimitive
final case class MapSndAcc(dt1: DataType,
                           dt2: DataType,
                           dt3: DataType,
                           f: Phrase[AccType ->: AccType],
                           record: Phrase[AccType]) extends AccPrimitive {
  f :: accT(dt3) ->: accT(dt2)
  record :: accT(dt1 x dt3)
  override val t: AccType = accT(dt1 x dt2)
}
