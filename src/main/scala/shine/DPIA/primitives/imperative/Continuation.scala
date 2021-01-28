package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive


@expPrimitive
final case class Continuation(dt: DataType,
                              cont: Phrase[(ExpType ->: CommType) ->: CommType])
  extends ExpPrimitive
{
  cont :: (expT(dt, read) ->: comm) ->: comm
  override val t: ExpType = expT(dt, read)
}