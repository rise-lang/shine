package rise.core.types

import arithexpr.arithmetic._
import rise.core.types

class NatIdentifier(
    override val name: String,
    override val range: Range = RangeUnknown,
    override val isExplicit: Boolean = false
) extends NamedVar(name, range)
    with types.Kind.Identifier
    with types.Kind.Explicitness {

  override lazy val toString: String = if (isExplicit) name else "_" + name

  override def copy(r: Range): NatIdentifier =
    new NatIdentifier(name, r, isExplicit)

  override def asExplicit: NatIdentifier = new NatIdentifier(name, range, true)

  override def asImplicit: NatIdentifier = new NatIdentifier(name, range, false)

  override def cloneSimplified(): NatIdentifier with SimplifiedExpr =
    new NatIdentifier(name, range, isExplicit) with SimplifiedExpr
}

object NatIdentifier {
  def apply(name: String, isExplicit: Boolean): NatIdentifier =
    new NatIdentifier(name, isExplicit = isExplicit)

  def apply(name: String): NatIdentifier = apply(name, isExplicit = false)

  def apply(name: String, range: Range, isExplicit: Boolean): NatIdentifier =
    new NatIdentifier(name, range, isExplicit = isExplicit)

  def apply(name: String, range: Range): NatIdentifier =
    apply(name, range, isExplicit = false)

  def apply(nv: NamedVar, isExplicit: Boolean): NatIdentifier =
    new NatIdentifier(nv.name, nv.range, isExplicit = isExplicit)

  def apply(nv: NamedVar): NatIdentifier = apply(nv, isExplicit = false)
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
