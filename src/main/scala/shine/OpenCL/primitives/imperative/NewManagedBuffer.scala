package shine.OpenCL.primitives.imperative

import shine.DPIA._
import shine.DPIA.Types._
import shine.DPIA.Phrases._
import shine.OpenCL.AccessFlags
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class NewManagedBuffer(dt: DataType,
                                  access: AccessFlags,
                                  k: Phrase[VarType ->: CommType]) extends CommandPrimitive {
  k :: varT(ManagedBufferType(dt)) ->: comm
}
