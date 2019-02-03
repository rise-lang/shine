package idealised.OpenCL.AST

import idealised.C.AST.BasicType
import idealised.DPIA.Nat

case class VectorType(n: Nat,
                      elemType: BasicType,
                      override val const: Boolean = false)
  extends BasicType(s"${elemType.name}$n")
