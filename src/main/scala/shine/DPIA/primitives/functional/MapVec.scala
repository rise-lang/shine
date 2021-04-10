package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class MapVec(n: Nat,
                        dt1: ScalarType,
                        dt2: ScalarType,
                        f: Phrase[ExpType ->: ExpType],
                        array: Phrase[ExpType]
                       ) extends ExpPrimitive {
  f :: expT(dt1, read) ->: expT(dt2, write)
  array :: expT(vec(n, dt1), read)
  override val t: ExpType = expT(vec(n, dt2), write)
}
