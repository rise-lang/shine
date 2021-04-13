package shine.OpenCL.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class New(a: AddressSpace,
                     dt: DataType,
                     f: Phrase[VarType ->: CommType]
                    ) extends CommandPrimitive {
  f :: varT(dt) ->: comm
}
