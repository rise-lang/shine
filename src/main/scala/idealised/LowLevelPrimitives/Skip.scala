package idealised.LowLevelPrimitives

import idealised.Core.OperationalSemantics._
import idealised.Core._

import scala.xml.Elem

case class Skip() extends CommandPrimitive {

  override def typeCheck(): Unit = { comm }

  override def eval(s: Store): Store = s

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[CommandType] = this

  override def prettyPrint: String = "skip"

  override def xmlPrinter: Elem = <skip/>
}
