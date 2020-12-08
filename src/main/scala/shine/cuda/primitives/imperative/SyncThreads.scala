package shine.cuda.primitives.imperative

import shine.DPIA.Phrases.{CommandPrimitive, Phrase, VisitAndRebuild}
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types.{CommType, comm}

import scala.xml.Elem

final case class SyncThreads() extends CommandPrimitive {

  override val t: CommType = comm

  override def prettyPrint: String = "__syncthreads()"

  override def xmlPrinter: Elem = <synchronize/>

  override def visitAndRebuild(
    f: VisitAndRebuild.Visitor
  ): Phrase[CommType] = this

  override def eval(s: Store): Store = ???
}
