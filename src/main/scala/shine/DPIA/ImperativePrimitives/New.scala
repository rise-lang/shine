package shine.DPIA.ImperativePrimitives

import shine.DPIA.DSL.identifier
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._

import scala.xml.Elem

final case class New(dt: DataType,
                     f: Phrase[VarType ->: CommType])
  extends CommandPrimitive {

  f :: varT(dt) ->: comm

  override def eval(s: Store): Store = {
    val f_ = OperationalSemantics.eval(s, f)
    val arg = identifier(freshName("x"), f.t.inT)
    val newStore = OperationalSemantics.eval(s + (arg.name -> 0), f_(arg))
    newStore - arg.name
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommType] = {
    New(fun.data(dt), VisitAndRebuild(f, fun))
  }

  override def prettyPrint: String = s"(new ${PrettyPhrasePrinter(f)})"

  override def xmlPrinter: Elem =
    <new dt={ToString(dt)}>
      {Phrases.xmlPrinter(f)}
    </new>
}
