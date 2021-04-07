package shine.cuda.primitives.imperative

import shine.DPIA.Phrases.CommandPrimitive
import shine.macros.Primitive.comPrimitive

/**
  * Synchronize all thread in a single thread block.
  */
@comPrimitive
final case class SyncThreads() extends CommandPrimitive {
  override def prettyPrint: String = "__syncthreads()"
}
