package shine.cuda.primitives.imperative

import shine.DPIA.Phrases.{CommandPrimitive, Phrase, VisitAndRebuild}
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types.{CommType, comm}

import scala.xml.Elem

final case class SyncWarp() extends CommandPrimitive {

  override val t: CommType = comm

  override def prettyPrint: String = "__syncwarp()"

  override def xmlPrinter: Elem = <synchronizeWarp/>

  override def visitAndRebuild(
                                f: VisitAndRebuild.Visitor
                              ): Phrase[CommType] = this

  override def eval(s: Store): Store = ???
}
