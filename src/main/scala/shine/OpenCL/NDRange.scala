package shine.OpenCL

import shine.DPIA.Nat
import shine.DPIA.Phrases.VisitAndRebuild

case class NDRange(x: Nat, y: Nat, z: Nat) {
  def apply(dim: Int): Nat = dim match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new Exception("Dimension too large; does not exist.")
  }

  def isEvaluable: Boolean = x.isEvaluable && y.isEvaluable && z.isEvaluable
  def ==(other: NDRange): Boolean = x == other.x && y == other.y && z == other.z

  def visitAndRebuild(f: VisitAndRebuild.Visitor): NDRange =
    NDRange(f.nat(x), f.nat(y), f.nat(z))
}

case class LocalSize(size: NDRange) {
  def visitAndRebuild(f: VisitAndRebuild.Visitor): LocalSize =
    LocalSize(size.visitAndRebuild(f))
}

case class GlobalSize(size: NDRange) {
  def visitAndRebuild(f: VisitAndRebuild.Visitor): GlobalSize =
    GlobalSize(size.visitAndRebuild(f))
}