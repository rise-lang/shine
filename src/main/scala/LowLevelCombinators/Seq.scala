package LowLevelCombinators

import Core.OperationalSemantics._
import Core._

import scala.xml.Elem

final case class Seq(c1: Phrase[CommandType],
                     c2: Phrase[CommandType])
  extends LowLevelCommCombinator {

  override def typeCheck(): Unit = {
    import TypeChecker._
    (c1 `:` comm) -> (c2 `:` comm) -> comm
  }

  override def eval(s: Store): Store = {
    val s1 = OperationalSemantics.eval(s, c1)
    OperationalSemantics.eval(s1, c2)
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    Seq(VisitAndRebuild(c1, fun), VisitAndRebuild(c2, fun))
  }

  override def prettyPrint: String =
    s"(${PrettyPrinter(c1)}; ${PrettyPrinter(c2)})"

  override def xmlPrinter: Elem =
    <seq>
      <c1>
        {Core.xmlPrinter(c1)}
      </c1>
      <c2>
        {Core.xmlPrinter(c2)}
      </c2>
    </seq>
}
