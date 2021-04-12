package shine.OpenCL.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
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

  override def eval(s: Store): Data = OperationalSemantics.eval(s, input)
}
