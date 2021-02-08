package shine.cuda.primitives.imperative

import shine.DPIA.Phrases.CommandPrimitive
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class SyncWarp() extends CommandPrimitive {
  override def prettyPrint: String = "__syncwarp()"
}
