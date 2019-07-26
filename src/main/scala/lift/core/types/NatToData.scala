package lift.core.types

import lift.arithmetic.NamedVar
import lift.core.{Nat, NatIdentifier, substitute}

sealed trait NatToData {
  def map(f:DataType => DataType): NatToData = this match {
    case ident: NatToDataIdentifier => ident
    case NatToDataLambda(x, body) => NatToDataLambda(x, f(body))
  }

  def apply(n: Nat): DataType = NatToDataApply(this, n)
}

case class NatToDataLambda private (x: NatIdentifier, body: DataType) extends NatToData {
  //See hash code of NatNatTypeFunction
  override def hashCode(): Int = this.apply(NamedVar("ComparisonDummy")).hashCode()

  override def apply(a: Nat): DataType = substitute(a, `for`=x, in=body)

  override def toString: String = s"($x: nat |-> $body)"

  override def equals(obj: Any): Boolean = obj match {
    case other:NatToDataLambda => body == other.apply(x)
    case _ => false
  }
}

final case class NatToDataIdentifier(name: String) extends NatToData with Kind.Identifier {
  override def toString: String = name
}
