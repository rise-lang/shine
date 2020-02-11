package shine.OpenCL

import arithexpr.arithmetic.{ArithExprFunctionCall, Range, RangeUnknown, Sign, SimplifiedExpr}
import shine.DPIA.Nat

// This class models OpenCL built in functions that can appear inside of arithmetic expressions
// examples are get_global_size(0), or get_local_id(1), but also OpenCL math functions, e.g., ceil or sin
class BuiltInFunctionCall private(name: String, val param: Int, range: Range)
  extends ArithExprFunctionCall(name, range) {

  override lazy val toString = s"$name($param)"

  override lazy val digest: Int = HashSeed ^ /*range.digest() ^*/ name.hashCode ^ param

  override val HashSeed = 0x31111111

  override def equals(that: Any): Boolean = that match {
    case f: BuiltInFunctionCall => this.name.equals(f.name) && this.param == f.param
    case _ => false
  }

  override lazy val (min : Nat, max: Nat) = (range.min.min, range.max.max)
  override lazy val sign: Sign.Value = Sign.Positive

  override def visitAndRebuild(f: Nat => Nat): Nat =
    f(new BuiltInFunctionCall(name, param, range.visitAndRebuild(f)))

  override def substitute(subs: collection.Map[Nat, Nat]): Option[Nat] =
    subs.get(this).orElse(
      range.substitute(subs).map(new BuiltInFunctionCall(name, param, _)))

  override def exposedArgs: Seq[Nat] = Seq()

  override def substituteExposedArgs(subMap: Map[Nat, SimplifiedExpr]): ArithExprFunctionCall = this

}

object BuiltInFunctionCall {
  def apply(name: String, param: Int, range: Range = RangeUnknown) : BuiltInFunctionCall =
    new BuiltInFunctionCall(name, param, range)
}
