package shine.OpenCL.AST

import shine.C
import shine.C.AST.{BasicType, Type}
import shine.DPIA.Nat
import shine.OpenCL.AddressSpace

case class VectorType(n: Nat,
                      elemType: BasicType,
                      override val const: Boolean = false)
  extends BasicType(s"${elemType.print}$n")

case class PointerType(a: AddressSpace, override val valueType: Type, override val const: Boolean = false)
  extends C.AST.PointerType(valueType, const) {
  override def print: String = s"$a ${valueType.print} *"
}
