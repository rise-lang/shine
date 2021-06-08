package rise.core.types

import arithexpr.arithmetic.ArithExpr

sealed trait NatToNat {
  def apply(n: Nat): Nat = NatToNatApply(this, n)
}

final case class NatToNatIdentifier(name: String) extends NatToNat {
  override def toString: String = name
}

final case class NatToNatLambda private (x: NatIdentifier, body: Nat)
    extends NatToNat {
  override def apply(l: Nat): Nat = ArithExpr.substitute(body, Map((x, l)))
  override def toString: String = s"($x: nat |-> $body)"
}
