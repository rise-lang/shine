package shine.DPIA.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._

import scala.xml.Elem

final case class ForNat(n: Nat,
                        body: Phrase[`(nat)->:`[CommType]],
                        unroll:Boolean)
  extends CommandPrimitive {

  {
    val k = body.t.x
    body :: k ->: comm
  }

  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommType] = {
    ForNat(fun.nat(n), VisitAndRebuild(body, fun), unroll)
  }

  override def prettyPrint: String = s"(forNat 0..$n ${PrettyPhrasePrinter(body)})"

  override def xmlPrinter: Elem =
    <forNat n={ToString(n)}>
      {Phrases.xmlPrinter(body)}
    </forNat>
}
