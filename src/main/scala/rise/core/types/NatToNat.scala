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
  override def equals(that: Any): Boolean = that match {
    case n2n: NatToNatIdentifier => this.name == n2n.name
    case _                       => false
  }
  override def hashCode(): Int = this.name.hashCode()
}

final case class NatToNatLambda private (x: NatIdentifier, body: Nat)
    extends NatToNat {
  // NatToNat have an interesting comparison behavior, as we do not define
  // equality for them as simple syntactic equality: we just want to make sure
  // their bodies are equal up-to renaming of the binder.

  // However, just updating equals is not sufficient, as many data structures,
  // such as HashMaps, use hashCodes as proxy for equality. In order to make
  // sure this property is respected, we ignore the identifier variable, and
  // just take the hash of the body evaluated at a known point
  override def hashCode(): Int =
    this(NatIdentifier("comparisonDummy", isExplicit = false)).hashCode()

  override def apply(l: Nat): Nat = ArithExpr.substitute(body, Map((x, l)))

  override def toString: String = s"($x: nat |-> $body)"

  override def equals(obj: Any): Boolean = obj match {
    case other: NatToNatLambda => body == other(x)
    case _                     => false
  }
}
