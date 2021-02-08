package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class Seq(c1: Phrase[CommType],
                     c2: Phrase[CommType])
  extends CommandPrimitive {
  c1 :: comm
  c2 :: comm

  override def eval(s: Store): Store = {
    val s1 = OperationalSemantics.eval(s, c1)
    OperationalSemantics.eval(s1, c2)
  }
}
