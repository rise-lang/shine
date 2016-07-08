package LowLevelCombinators

import Core.OperationalSemantics._
import Core._

import scala.xml.Elem

case class Skip() extends LowLevelCommCombinator {

  override def typeCheck(): Unit = {}

  override def eval(s: Store): Store = s

  override def visitAndRebuild(f: VisitAndRebuild.fun): Phrase[CommandType] = this

  override def prettyPrint: String = "skip"

  override def xmlPrinter: Elem = <skip/>
}
