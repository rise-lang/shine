package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Let(dt1: DataType,
                     dt2: DataType,
                     access: AccessType,
                     value: Phrase[ExpType],
                     f: Phrase[ExpType ->: ExpType]
                    ) extends ExpPrimitive {
  value :: expT(dt1, read)
  f :: expT(dt1, read) ->: expT(dt2, access)
  override val t: ExpType = expT(dt2, access)
}
