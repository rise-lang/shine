package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types._

// not final (and not using the macro) because of DSL.typed.skip
case class Skip() extends CommandPrimitive {

  override val t: CommType = comm

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[CommType] = this

  override def prettyPrint: String = "skip"
}
