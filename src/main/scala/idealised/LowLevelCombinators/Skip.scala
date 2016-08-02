package idealised.LowLevelCombinators

import idealised.Core.OperationalSemantics._
import idealised.Core._

import scala.xml.Elem

case class Skip() extends LowLevelCommCombinator {

  override def typeCheck(): Unit = { comm }

  override def eval(s: Store): Store = s

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[CommandType] = this

  override def prettyPrint: String = "skip"

  override def xmlPrinter: Elem = <skip/>
}
