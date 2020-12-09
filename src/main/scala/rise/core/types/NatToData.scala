package rise.core.types

import rise.core._

sealed trait NatCollectionToData {
  def map(f: DataType => DataType): NatCollectionToData = this match {
    case ident: NatCollectionToDataIdentifier => ident
    case NatCollectionToDataLambda(x, body)   => NatCollectionToDataLambda(x, f(body))
  }

  def apply(ns: NatCollection): DataType = this match {
    case NatCollectionToDataLambda(x, body) => substitute.natCollectionInType(ns, x, body)
    case _ => NatCollectionToDataApply(this, ns)
  }
}

object NatCollectionToData {
  def apply(f: NatCollectionIdentifier => DataType):NatCollectionToData = {
    val id = NatCollectionIdentifier(freshName("ns")) //Non explicit for type inference reasons
    NatCollectionToDataLambda(id, f(id))
  }
}

final case class NatCollectionToDataIdentifier(name: String,
                                     override val isExplicit: Boolean = false
                                    ) extends NatCollectionToData
  with Kind.Identifier
  with Kind.Explicitness {
  override def toString: String = if (isExplicit) name else "_" + name
  override def asExplicit: NatCollectionToDataIdentifier = this.copy(isExplicit = true)
  override def asImplicit: NatCollectionToDataIdentifier = this.copy(isExplicit = false)
  override def equals(that: Any): Boolean = that match {
    case n2d: NatCollectionToDataIdentifier => this.name == n2d.name
    case _                        => false
  }
  override def hashCode(): Int = this.name.hashCode()
}


case class NatCollectionToDataLambda private (x: NatCollectionIdentifier, body: DataType)
  extends NatCollectionToData {
  // See hash code of NatNatTypeFunction
  override def hashCode(): Int =
    this.apply(NatCollectionIdentifier("ComparisonDummy")).hashCode()

  override def apply(a: NatCollection): DataType =
    substitute.natCollectionInType(a, `for` = x, in = body)

  override def toString: String = s"($x: nats |-> $body)"

  override def equals(obj: Any): Boolean = obj match {
    case other: NatCollectionToDataLambda => body == other.apply(x)
    case _                      => false
  }
}


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
  override def equals(that: Any): Boolean = that match {
    case n2d: NatToDataIdentifier => this.name == n2d.name
    case _                        => false
  }
  override def hashCode(): Int = this.name.hashCode()
}

case class NatToDataLambda private (x: NatIdentifier, body: DataType)
    extends NatToData{
  // See hash code of NatNatTypeFunction
  override def hashCode(): Int =
    this.apply(NatIdentifier("ComparisonDummy")).hashCode()

  override def apply(a: Nat): DataType =
    substitute.natInDataType(a, `for` = x, in = body)

  override def toString: String = s"($x: nat |-> $body)"

  override def equals(obj: Any): Boolean = obj match {
    case other: NatToDataLambda => body == other.apply(x)
    case _                      => false
  }
}
