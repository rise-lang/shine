package shine.DPIA.ImperativePrimitives

import shine.DPIA.Phrases
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._

import scala.xml.Elem

final case class Seq(c1: Phrase[CommType],
                     c2: Phrase[CommType])
  extends CommandPrimitive {

  c1 :: comm
  c2 :: comm

  override def eval(s: Store): Store = {
    val s1 = OperationalSemantics.eval(s, c1)
    OperationalSemantics.eval(s1, c2)
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommType] = {
    Seq(VisitAndRebuild(c1, fun), VisitAndRebuild(c2, fun))
  }

  override def prettyPrint: String =
    s"(${PrettyPhrasePrinter(c1)}; ${PrettyPhrasePrinter(c2)})"

  override def xmlPrinter: Elem =
    <seq>
      <c1>
        {Phrases.xmlPrinter(c1)}
      </c1>
      <c2>
        {Phrases.xmlPrinter(c2)}
      </c2>
    </seq>
}
