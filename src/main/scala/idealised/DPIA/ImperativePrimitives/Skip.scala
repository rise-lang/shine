package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.utils._
import idealised.DPIA.Phrases.{CommandPrimitive, Phrase, VisitAndRebuild}
import idealised.DPIA.Types.{CommandType, comm}
import idealised.DPIA._

import scala.xml.Elem

// not final because of DSL.typed.skip
case class Skip() extends CommandPrimitive {

  override def typeCheck(): Unit = { comm }

  override def eval(s: Store): Store = s

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[CommandType] = this

  override def prettyPrint: String = "skip"

  override def xmlPrinter: Elem = <skip/>
}
