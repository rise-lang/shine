package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class ToMem(dt: DataType,
                       input: Phrase[ExpType]
                      ) extends ExpPrimitive {
  input :: expT(dt, write)
  override val t: ExpType = expT(dt, read)
}
