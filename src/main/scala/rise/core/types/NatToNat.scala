package rise.core.types

import arithexpr.arithmetic.{ArithExpr, ArithExprFunctionCall, RangeAdd, SimplifiedExpr}
import rise.core.freshName

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

  //NatNatTypeFunction have an interesting comparison behavior, as we do not define
  //equality for them as simple syntactic equality: we just want to make sure their bodies
  //are equal up-to renaming of the binder.

  //However, just updating equals is not sufficient, as many data structures, such as HashMaps,
  //use hashCodes as proxy for equality. In order to make sure this property is respected, we ignore
  //the identifier variable, and just take the hash of the body evaluated at a known point
  override def hashCode(): Int = apply(NatIdentifier("comparisonDummy")).hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case other: NatToNatLambda => body == other.apply(x)
    case _ => false
  }
}

object NatToNatLambda {
  def apply(upperBound:Nat, f: NatIdentifier => Nat):NatToNatLambda = {
    val x = NatIdentifier(freshName("n"), RangeAdd(0, upperBound, 1))
    NatToNatLambda(x, f(x))
  }

  def apply(upperBound:Nat, id: NatIdentifier, body:Nat):NatToNatLambda = {
    val x = NatIdentifier(freshName("n"), RangeAdd(0, upperBound, 1))
    NatToNatLambda(x, x => ArithExpr.substitute(body, Map((id, x))))
  }

  def apply(range: arithexpr.arithmetic.Range, id: NatIdentifier, body: Nat): NatToNatLambda = {
    val x = NatIdentifier(freshName("n"), range)
    NatToNatLambda(x, x => ArithExpr.substitute(body, Map((id, x))))
  }
}
