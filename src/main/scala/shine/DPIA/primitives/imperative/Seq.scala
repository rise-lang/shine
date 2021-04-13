package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class Seq(c1: Phrase[CommType],
                     c2: Phrase[CommType])
  extends CommandPrimitive {
  c1 :: comm
  c2 :: comm
}
