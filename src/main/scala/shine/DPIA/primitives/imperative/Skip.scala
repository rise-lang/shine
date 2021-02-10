package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._

import scala.xml.Elem

// not final (and not using the macro) because of DSL.typed.skip
case class Skip() extends CommandPrimitive {

  override val t: CommType = comm

  override def eval(s: Store): Store = s

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[CommType] = this

  override def prettyPrint: String = "skip"

  override def xmlPrinter: Elem = <skip/>
}
