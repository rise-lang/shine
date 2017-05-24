package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types.{CommandType, ExpType, IndexType, comm}
import idealised.DPIA._

import scala.xml.Elem

final case class For(n: Nat,
                     body: Phrase[ExpType -> CommandType])
  extends CommandPrimitive {

  override def typeCheck(): Unit = {
    import idealised.DPIA.Types.TypeChecker._
    (n: Nat) -> (body :: t"exp[idx($n)] -> comm") -> comm
  }

  override def eval(s: Store): Store = {
    val nE = evalIndexExp(s, Literal(IndexData(n), IndexType(n)))
    val bodyE = OperationalSemantics.eval(s, body)
    (0 until nE.eval).foldLeft(s)((s1, i) =>
      OperationalSemantics.eval(s1, bodyE(Literal(i, IndexType(n))))
    )
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    For(fun(n), VisitAndRebuild(body, fun))
  }

  override def prettyPrint: String = s"(for 0..$n ${PrettyPhrasePrinter(body)})"

  override def xmlPrinter: Elem =
    <for n={ToString(n)}>
      {Phrases.xmlPrinter(body)}
    </for>
}
