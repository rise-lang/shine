package shine.DPIA.ImperativePrimitives

import shine.DPIA.FunctionalPrimitives.AsIndex
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class For(n: Nat,
                     body: Phrase[ExpType ->: CommType],
                     unroll:Boolean)
  extends CommandPrimitive {

  body :: expT(idx(n), read) ->: comm

  override def eval(s: Store): Store = {
    val nE = evalIndexExp(s, AsIndex(n, Natural(n)))
    val bodyE = OperationalSemantics.eval(s, body)
    (0 until nE.eval).foldLeft(s)((s1, i) =>
      OperationalSemantics.eval(s1, bodyE(Literal(i)))
    )
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommType] = {
    For(fun.nat(n), VisitAndRebuild(body, fun), unroll)
  }

  override def prettyPrint: String = s"(for 0..$n ${PrettyPhrasePrinter(body)})"

  override def xmlPrinter: Elem =
    <for n={ToString(n)}>
      {Phrases.xmlPrinter(body)}
    </for>
}
