package idealised.OpenCL.ImperativePrimitives

import idealised.DPIA.DSL.identifier
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.AddressSpace

import scala.xml.Elem

final case class OpenCLNew(dt: DataType,
                           addressSpace: AddressSpace,
                           f: Phrase[VarType ->: CommType]) extends CommandPrimitive {

  override val t: CommType =
    (dt: DataType) ->: (addressSpace: AddressSpace) ->:
      (f :: t"var[$dt] -> comm") ->: comm

  override def eval(s: Store): Store = {
    val f_ = OperationalSemantics.eval(s, f)
    val arg = identifier(freshName("x"), f.t.inT)
    val newStore = OperationalSemantics.eval(s + (arg.name -> 0), f_(arg))
    newStore - arg.name
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommType] = {
    OpenCLNew(fun.data(dt), addressSpace, VisitAndRebuild(f, fun))
  }

  override def prettyPrint: String = s"(new $addressSpace ${PrettyPhrasePrinter(f)})"

  override def xmlPrinter: Elem =
    <new dt={ToString(dt)} addressSpace={ToString(addressSpace)}>
      {Phrases.xmlPrinter(f)}
    </new>
}
