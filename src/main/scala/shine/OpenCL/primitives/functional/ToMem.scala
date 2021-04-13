package shine.OpenCL.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class ToMem(addrSpace: AddressSpace,
                       dt: DataType,
                       input: Phrase[ExpType]
                      ) extends ExpPrimitive {
  input :: expT(dt, write)
  override val t: ExpType = expT(dt, read)
}
