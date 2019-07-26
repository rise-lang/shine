package lift.core.types

import lift.arithmetic.{ArithExpr, ArithExprFunction, NamedVar}
import lift.core.{Nat, NatIdentifier}

sealed trait NatToNat {
  def apply(n: Nat): Nat = NatToNatApply(this, n)
}

final case class NatToNatLambda private(x: NatIdentifier, body: Nat) extends NatToNat {
  // NatToNat have an interesting comparison behavior, as we do not define
  // equality for them as simple syntactic equality: we just want to make sure their bodies
  // are equal up-to renaming of the binder.

  // However, just updating equals is not sufficient, as many data structures, such as HashMaps,
  // use hashCodes as proxy for equality. In order to make sure this property is respected, we ignore
  // the identifier variable, and just take the hash of the body evaluated at a known point
  override def hashCode(): Int = this(NamedVar("comparisonDummy")).hashCode()

  override def apply(l: Nat): Nat = ArithExpr.substitute(body, Map((x, l)))

  override def toString: String = s"($x: nat |-> $body)"

  override def equals(obj: Any): Boolean = obj match {
    case other: NatToNatLambda => body == other(x)
    case _ => false
  }
}

final case class NatToNatIdentifier(name: String) extends NatToNat with Kind.Identifier {
  override lazy val toString: String = name
}

final class NatToNatApply(val f: NatToNat, val n: Nat) extends ArithExprFunction(s"$f($n)") {
  override def visitAndRebuild(f: Nat => Nat): Nat = this
  override lazy val toString: String = s"$f($n)"
}

object NatToNatApply {
  def apply(f: NatToNat, n: Nat): Nat = f match {
    case l: NatToNatLambda      => l.apply(n)
    case i: NatToNatIdentifier  => new NatToNatApply(i, n)
  }
  def unapply(arg: NatToNatApply): Option[(NatToNat, Nat)] = Some((arg.f, arg.n))
}
