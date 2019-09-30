package idealised.OpenCL.AST

import idealised.C
import idealised.C.AST.{BasicType, Type}
import idealised.DPIA.Nat
import idealised.OpenCL.AddressSpace

case class VectorType(n: Nat,
                      elemType: BasicType,
                      override val const: Boolean = false)
  extends BasicType(s"${elemType.name}$n")

case class PointerType(a: AddressSpace, override val valueType: Type, override val const: Boolean = false)
  extends C.AST.PointerType(valueType, const) {
  override def print: String = s"$a ${valueType.print} *"
}
