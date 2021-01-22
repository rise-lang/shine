package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases.{CommandPrimitive, Phrase, VisitAndRebuild}
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types.{CommType, comm}

import scala.xml.Elem

final case class Comment(comment : String) extends CommandPrimitive {

  override val t: CommType = comm

  override def eval(s: Store): Store = s

  override def prettyPrint: String = s"\n//$comment\n"

  override def xmlPrinter: Elem = <comment>{comment}</comment>

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[CommType] = this
}
