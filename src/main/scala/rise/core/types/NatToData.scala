package rise.core.types

import rise.arithmetic.NamedVar
import rise.core._

sealed trait NatToData {
  def map(f:DataType => DataType): NatToData = this match {
    case ident: NatToDataIdentifier => ident
    case NatToDataLambda(x, body) => NatToDataLambda(x, f(body))
  }

  def apply(n: Nat): DataType = NatToDataApply(this, n)
}

case class NatToDataLambda private (x: NatIdentifier, body: DataType) extends NatToData {
  //See hash code of NatNatTypeFunction
  override def hashCode(): Int = this.apply(NatIdentifier("ComparisonDummy")).hashCode()

  override def apply(a: Nat): DataType = substitute.natInDataType(a, `for`=x, in=body)

  override def toString: String = s"($x: nat |-> $body)"

  override def equals(obj: Any): Boolean = obj match {
    case other:NatToDataLambda => body == other.apply(x)
    case _ => false
  }
}

final case class NatToDataIdentifier(name: String, override val isExplicit: Boolean = false)
  extends NatToData with Kind.Identifier with Kind.Explicitness {
  override def toString: String = if (isExplicit) name else "_" + name
  override def asExplicit: NatToDataIdentifier = this.copy(isExplicit = true)
  override def asImplicit: NatToDataIdentifier = this.copy(isExplicit = false)
  override def equals(that: Any): Boolean = that match {
    case n2d: NatToDataIdentifier => this.name == n2d.name
    case _ => false
  }
}
