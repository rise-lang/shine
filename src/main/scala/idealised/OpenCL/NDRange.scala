package idealised.OpenCL

import idealised.DPIA.Nat

case class NDRange(x: Nat, y: Nat, z: Nat) {
  def isEvaluable: Boolean = x.isEvaluable && y.isEvaluable && z.isEvaluable
  def ==(other: NDRange): Boolean = x == other.x && y == other.y && z == other.z
}
