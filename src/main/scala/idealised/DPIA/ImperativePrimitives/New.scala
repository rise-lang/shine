package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.DSL.identifier
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class New(dt: DataType,
                     addressSpace: AddressSpace,
                     f: Phrase[(ExpType x AccType) -> CommandType])
  extends CommandPrimitive {

  override def typeCheck(): Unit = {
    import TypeChecker._
    (dt: DataType) -> /* (addressSpace: AddressSpace) -> */
      (f :: t"var[$dt] -> comm") -> comm
  }

  override def eval(s: Store): Store = {
    val f_ = OperationalSemantics.eval(s, f)
    val arg = identifier(newName(), f.t.inT)
    val newStore = OperationalSemantics.eval(s + (arg.name -> 0), f_(arg))
    newStore - arg.name
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    New(fun(dt), addressSpace, VisitAndRebuild(f, fun))
  }

  override def prettyPrint: String = s"(new $addressSpace ${PrettyPhrasePrinter(f)})"

  override def xmlPrinter: Elem =
    <new dt={ToString(dt)} addressspace={ToString(addressSpace)}>
      {Phrases.xmlPrinter(f)}
    </new>
}
