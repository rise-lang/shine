package rise.core.types

import arithexpr.arithmetic.{NamedVar, RangeAdd}
import rise.core._
import rise.core.types.DataType._

sealed trait NatToData {
  def map(f: DataType => DataType): NatToData = this match {
    case ident: NatToDataIdentifier => ident
    case NatToDataLambda(x, body)   => NatToDataLambda(x, f(body))
  }

  def apply(n: Nat): DataType = NatToDataApply(this, n)
}

final case class NatToDataIdentifier(name: String) extends NatToData {
  override def toString: String = name
}

case class NatToDataLambda private (x: NatIdentifier, body: DataType)
    extends NatToData {
  // See hash code of NatNatTypeFunction
  override def apply(a: Nat): DataType =
    substitute.natInDataType(a, `for` = x, in = body)

  override def toString: String = s"($x: nat |-> $body)"

  //See hash code of NatNatTypeFunction
  override def hashCode(): Int = this.apply(NamedVar("ComparisonDummy")).hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case other:NatToDataLambda => body == other.apply(x)
    case _ => false
  }
}

object NatToDataLambda {
  def apply(upperBound: Nat, f: NatIdentifier => DataType): NatToDataLambda = {
    val n = NatIdentifier(freshName("n"), RangeAdd(0, upperBound, 1))
    NatToDataLambda(n, f(n))
  }
}
