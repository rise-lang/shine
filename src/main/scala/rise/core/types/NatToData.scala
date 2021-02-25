package rise.core.types

import rise.core._

sealed trait NatToData {
  def map(f: DataType => DataType): NatToData = this match {
    case ident: NatToDataIdentifier => ident
    case NatToDataLambda(x, body)   => NatToDataLambda(x, f(body))
  }

  def apply(n: Nat): DataType = NatToDataApply(this, n)
}

final case class NatToDataIdentifier(name: String,
                                     override val isExplicit: Boolean = false
                                    ) extends NatToData
  with Kind.Identifier
  with Kind.Explicitness {
  override def toString: String = if (isExplicit) name else "_" + name
  override def asExplicit: NatToDataIdentifier = this.copy(isExplicit = true)
  override def asImplicit: NatToDataIdentifier = this.copy(isExplicit = false)
}

case class NatToDataLambda private (x: NatIdentifier, body: DataType)
    extends NatToData {
  // See hash code of NatNatTypeFunction
  override def apply(a: Nat): DataType =
    substitute.natInDataType(a, `for` = x, in = body)

  override def toString: String = s"($x: nat |-> $body)"
}
