package rise.core.types

import arithexpr.arithmetic.ArithExpr

sealed trait NatToNat {
  def apply(n: Nat): Nat = NatToNatApply(this, n)
}

final case class NatToNatIdentifier(name: String,
                                    override val isExplicit: Boolean = false
                                   ) extends NatToNat
  with Kind.Identifier
  with Kind.Explicitness {
  override def toString: String = if (isExplicit) name else "_" + name
  override def asExplicit: NatToNatIdentifier = this.copy(isExplicit = true)
  override def asImplicit: NatToNatIdentifier = this.copy(isExplicit = false)
}

final case class NatToNatLambda private (x: NatIdentifier, body: Nat)
    extends NatToNat {
  override def apply(l: Nat): Nat = ArithExpr.substitute(body, Map((x, l)))
  override def toString: String = s"($x: nat |-> $body)"
}
