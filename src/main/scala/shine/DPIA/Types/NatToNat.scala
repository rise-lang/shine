package shine.DPIA.Types

import arithexpr.arithmetic._
import shine.DPIA.{Nat, NatIdentifier, freshName}

sealed trait NatToNat  {
  def apply(n: Nat): Nat = NatToNatApply(this, n)
}

final case class NatToNatLambda private(x: NatIdentifier, body: Nat) extends NatToNat {
  //NatNatTypeFunction have an interesting comparison behavior, as we do not define
  //equality for them as simple syntactic equality: we just want to make sure their bodies
  //are equal up-to renaming of the binder.

  //However, just updating equals is not sufficient, as many data structures, such as HashMaps,
  //use hashCodes as proxy for equality. In order to make sure this property is respected, we ignore
  //the identifier variable, and just take the hash of the body evaluated at a known point
  override def hashCode(): Int = apply(NatIdentifier("comparisonDummy")).hashCode()

  override def apply(n: Nat): Nat = ArithExpr.substitute(body, Map((x, n)))

  override def toString: String = s"($x: nat |-> $body)"

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
    val x = NamedVar(freshName("n"), RangeAdd(0, upperBound, 1))
    NatToNatLambda(x, x => ArithExpr.substitute(body, Map((id, x))))
  }
}

final case class NatToNatIdentifier(name: String) extends NatToNat with Kind.Identifier {
  override lazy val toString: String = name
}

final class NatToNatApply(val f: NatToNat, val n: Nat) extends ArithExprFunctionCall(s"$f($n)") {
  override def visitAndRebuild(f: Nat => Nat): Nat = this
  override lazy val toString: String = s"$f($n)"

  override def exposedArgs: Seq[Nat] = Seq(n)

  override def substituteExposedArgs(subMap: Map[Nat, SimplifiedExpr]): ArithExprFunctionCall =
    new NatToNatApply(f, subMap.getOrElse(n, n))

  override def substitute(subs: collection.Map[Nat, Nat]): Option[Nat] = ???
}

object NatToNatApply {
  def apply(f: NatToNat, n: Nat): Nat = f match {
    case l: NatToNatLambda      => l.apply(n)
    case i: NatToNatIdentifier  => new NatToNatApply(i, n)
  }
  def unapply(arg: NatToNatApply): Option[(NatToNat, Nat)] = Some((arg.f, arg.n))
}