package shine.OpenCL.primitives.imperative

import shine.DPIA._
import shine.DPIA.Types._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.OpenCL.AccessFlags

import scala.xml.Elem

final case class NewManagedBuffer(dt: DataType,
                                  access: AccessFlags,
                                  k: Phrase[VarType ->: CommType]) extends CommandPrimitive {
  k :: varT(ManagedBufferType(dt)) ->: comm

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[CommType] =
    NewManagedBuffer(f.data(dt), access, VisitAndRebuild(k, f))

  override def eval(s: Store): Store = ???
  override def prettyPrint: String = ???
  override def xmlPrinter: Elem = ???
}
