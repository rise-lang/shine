package rise.core.types

import rise.core._

sealed trait NatToData {
  def map(f: DataType => DataType): NatToData = this match {
    case ident: NatToDataIdentifier => ident
    case NatToDataLambda(x, body)   => NatToDataLambda(x, f(body))
  }

  def apply(n: Nat): DataType = NatToDataApply(this, n)
}

final case class NatToDataIdentifier(name: String) extends NatToData with Kind.Identifier

case class NatToDataLambda private (x: NatIdentifier, body: DataType)
    extends NatToData {
  // See hash code of NatNatTypeFunction
  override def apply(a: Nat): DataType =
    substitute.natInDataType(a, `for` = x, in = body)

  override def toString: String = s"($x: nat |-> $body)"
}
