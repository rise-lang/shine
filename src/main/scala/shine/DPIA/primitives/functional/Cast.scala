package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Cast(dt1: BasicType,
                      dt2: BasicType,
                      e: Phrase[ExpType]
                     )extends ExpPrimitive {
  e :: expT(dt1, read)
  override val t: ExpType = expT(dt2, read)
}
