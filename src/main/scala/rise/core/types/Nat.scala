package rise.core.types

import arithexpr.arithmetic._
import rise.core.types

class NatIdentifier(
    override val name: String,
    override val range: Range,
    override val isExplicit: Boolean,
    val isTuningParam: Boolean
) extends NamedVar(name, range)
    with types.Kind.Identifier
    with types.Kind.Explicitness {
  override lazy val toString: String = if (isExplicit) name else "_" + name

  override def copy(r: Range): NatIdentifier =
    new NatIdentifier(name, r, isExplicit, isTuningParam)

  override def asExplicit: NatIdentifier = new NatIdentifier(name, range, true, isTuningParam)

  override def asImplicit: NatIdentifier = new NatIdentifier(name, range, false, isTuningParam)

  override def substitute(subs: collection.Map[ArithExpr, ArithExpr]): Option[ArithExpr] = {
    subs.get(this).orElse(range.substitute(subs)
      .map(NatIdentifier(name, _, isExplicit, isTuningParam)))
  }

  override def visitAndRebuild(f: ArithExpr => ArithExpr): ArithExpr = {
    f(new NatIdentifier(name, range.visitAndRebuild(f), isExplicit, isTuningParam))
  }

  override def cloneSimplified(): NatIdentifier with SimplifiedExpr =
    new NatIdentifier(name, range, isExplicit, isTuningParam) with SimplifiedExpr
}

object NatIdentifier {
  def apply(name: String, range: Range, isExplicit: Boolean, isTuningParam: Boolean): NatIdentifier =
    new NatIdentifier(name, range, isExplicit = isExplicit, isTuningParam = isTuningParam)

  def apply(name: String, range: Range, isExplicit: Boolean): NatIdentifier =
    new NatIdentifier(name, range, isExplicit = isExplicit, isTuningParam = false)

  def apply(name: String, isExplicit: Boolean): NatIdentifier =
    apply(name, RangeUnknown, isExplicit)

  def apply(name: String, range: Range): NatIdentifier =
    apply(name, range, isExplicit = false)

  def apply(name: String): NatIdentifier =
    apply(name, isExplicit = false)

  def apply(nv: NamedVar, isExplicit: Boolean, isTuningParam: Boolean): NatIdentifier =
    new NatIdentifier(nv.name, nv.range, isExplicit, isTuningParam)
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
