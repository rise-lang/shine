package idealised.ImperativePrimitives

import idealised.Core.OperationalSemantics._
import idealised.Core._

import scala.xml.Elem

// not final because of DSL.typed.skip
case class Skip() extends CommandPrimitive {

  override def typeCheck(): Unit = { comm }

  override def eval(s: Store): Store = s

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[CommandType] = this

  override def prettyPrint: String = "skip"

  override def xmlPrinter: Elem = <skip/>
}
