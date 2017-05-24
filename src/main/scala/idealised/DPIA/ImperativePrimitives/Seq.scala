package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types.{CommandType, comm}
import idealised.DPIA.{Phrases, _}

import scala.xml.Elem

final case class Seq(c1: Phrase[CommandType],
                     c2: Phrase[CommandType])
  extends CommandPrimitive {

  override def typeCheck(): Unit = {
    import idealised.DPIA.Types.TypeChecker._
    (c1 :: comm) -> (c2 :: comm) -> comm
  }

  override def eval(s: Store): Store = {
    val s1 = OperationalSemantics.eval(s, c1)
    OperationalSemantics.eval(s1, c2)
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
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
