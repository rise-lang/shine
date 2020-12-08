package shine.cuda

import arithexpr.arithmetic.{
  ArithExprFunctionCall, Range, RangeUnknown, Sign, SimplifiedExpr}
import shine.DPIA.Nat
import shine.OpenCL.BuiltInFunctionCall

class BuiltInAttribute private(name: String, val param: Char, range: Range)
  extends ArithExprFunctionCall(name, range) {

  override lazy val toString = s"$name.$param"

  override lazy val digest: Int = HashSeed ^ name.hashCode ^ param

  override val HashSeed = 0x31111111

  override def equals(that: Any): Boolean = that match {
    case f: BuiltInAttribute =>
      this.name.equals(f.name) && this.param == f.param
    case _ => false
  }

  override lazy val (min : Nat, max: Nat) = (range.min.min, range.max.max)
  override lazy val sign: Sign.Value = Sign.Positive

  override def visitAndRebuild(f: Nat => Nat): Nat =
    f(new BuiltInAttribute(name, param, range.visitAndRebuild(f)))

  override def substitute(subs: collection.Map[Nat, Nat]): Option[Nat] =
    subs.get(this).orElse(
      range.substitute(subs).map(new BuiltInAttribute(name, param, _)))

  override def exposedArgs: Seq[Nat] = Seq()

  override def substituteExposedArgs(
    subMap: Map[Nat, SimplifiedExpr]
  ): ArithExprFunctionCall = this

}

object BuiltInAttribute {
  def apply(
    name: String,
    param: Char,
    range: Range = RangeUnknown
  ): BuiltInAttribute =
    new BuiltInAttribute(name, param, range)
}
