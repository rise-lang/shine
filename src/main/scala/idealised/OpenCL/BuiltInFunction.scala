package idealised.OpenCL

import idealised.DPIA.Nat
import lift.arithmetic.{ArithExprFunction, Range, RangeUnknown, Sign}

class BuiltInFunction private(name: String, val param: Int, range: Range)
  extends ArithExprFunction(name, range) {

  override lazy val toString = s"$name($param)"

  override lazy val digest: Int = HashSeed ^ /*range.digest() ^*/ name.hashCode ^ param

  override val HashSeed = 0x31111111

  override def equals(that: Any): Boolean = that match {
    case f: BuiltInFunction => this.name.equals(f.name) && this.param == f.param
    case _ => false
  }

  override lazy val (min : Nat, max: Nat) = (range.min.min, range.max.max)
  override lazy val sign: Sign.Value = Sign.Positive

  override def visitAndRebuild(f: Nat => Nat): Nat =
    f(new BuiltInFunction(name, param, range.visitAndRebuild(f)))

}

object BuiltInFunction {
  def apply(name: String, param: Int, range: Range = RangeUnknown) : BuiltInFunction =
    new BuiltInFunction(name, param, range)
}
