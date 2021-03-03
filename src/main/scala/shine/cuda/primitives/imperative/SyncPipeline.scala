package shine.cuda.primitives.imperative

import shine.DPIA.Phrases.{CommandPrimitive, Phrase}
import shine.DPIA.Types.{ExpType, pipeline, read}
import shine.DPIA.expT
import shine.macros.Primitive.comPrimitive

/**
  * Execute and wait for all asynchronous memory transactions (used by the {@link GlobalToSharedAcc})
  */
@comPrimitive
final case class SyncPipeline(pipe: Phrase[ExpType]) extends CommandPrimitive {
  pipe :: expT(pipeline, read)

  override def prettyPrint: String = s"$pipe.commit_and_wait()"
}
