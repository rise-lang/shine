package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class MapSnd(w: AccessType,
                        dt1: DataType,
                        dt2: DataType,
                        dt3: DataType,
                        f: Phrase[ExpType ->: ExpType],
                        record: Phrase[ExpType]
                       ) extends ExpPrimitive {
  f :: expT(dt2, w) ->: expT(dt3, w)
  record :: expT(dt1 x dt2, w)
  override val t: ExpType = expT(dt1 x dt3, w)
}
