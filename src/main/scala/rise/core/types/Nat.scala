package rise.core.types

import arithexpr.arithmetic._
import rise.core.types


class NatIdentifier(override val name: String, override val range: Range = RangeUnknown)
  extends NamedVar(name, range) with types.Kind.Identifier {
  override lazy val toString: String = name
  override def copy(r: Range): NatIdentifier = new NatIdentifier(name, r)
  override def cloneSimplified(): NatIdentifier with SimplifiedExpr =
    new NatIdentifier(name, range) with SimplifiedExpr
}

object NatIdentifier {
  def apply(name: String): NatIdentifier = new NatIdentifier(name)
  def apply(name: String, range: Range): NatIdentifier = new NatIdentifier(name, range)
  def apply(nv: NamedVar): NatIdentifier = new NatIdentifier(nv.name, nv.range)
}

final class NatToNatApply(val f: NatToNat, val n: Nat)
  extends ArithExprFunctionCall(s"$f($n)") {
  override def visitAndRebuild(fun: Nat => Nat): Nat =
    fun(NatToNatApply(this.f, fun(n)))

  override def substitute(subs: collection.Map[ArithExpr, ArithExpr]
                         ): Option[ArithExpr] =
    // TODO? subs in 'f'
    n.substitute(subs).map(NatToNatApply(f, _))

  override lazy val toString: String = s"$f($n)"

  override def freeVariables: Set[Var] =
    // TODO? ArithExpr.freeVariables(f)
    ArithExpr.freeVariables(n)

  override def exposedArgs: Seq[Nat] = Seq(n)

  override def substituteExposedArgs(
                                      subMap: Map[Nat, SimplifiedExpr]
                                    ): ArithExprFunctionCall =
    new NatToNatApply(f, subMap.getOrElse(n, n))
}

object NatToNatApply {
  def apply(f: NatToNat, n: Nat): Nat = f match {
    case l: NatToNatLambda     => l.apply(n)
    case i: NatToNatIdentifier => new NatToNatApply(i, n)
  }
  def unapply(arg: NatToNatApply): Option[(NatToNat, Nat)] =
    Some((arg.f, arg.n))
}
