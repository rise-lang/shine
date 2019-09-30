package idealised.DPIA.Types

import idealised.DPIA.{Nat, NatIdentifier, freshName}
import lift.arithmetic.{NamedVar, RangeAdd}

sealed trait NatToData {
  def apply(n: Nat): DataType = NatToDataApply(this, n)
}

case class NatToDataLambda(x:NatIdentifier, body:DataType) extends NatToData {
  //See hash code of NatNatTypeFunction
  override def hashCode(): Int = this.apply(NamedVar("ComparisonDummy")).hashCode()

  override def apply(n:Nat): DataType = DataType.substitute(n, `for`=x, `in`=body)

  override def toString: String = s"($x: nat |-> $body)"

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

final case class NatToDataIdentifier(name: String) extends NatToData with Kind.Identifier {
  override def toString: String = name
}

object NatToData {
  def substitute[T <: NatToData](ae: Nat, `for`: NatIdentifier, in: T): T = {
    (in match {
      case NatToDataLambda(x, body) =>
        val nX: NatIdentifier = ae match {
          case aeI: NatIdentifier => Nat.substitute(aeI, `for`, x).asInstanceOf[NatIdentifier]
          case _ => x
        }
        NatToDataLambda(nX, DataType.substitute(ae, `for`, body))
      case i: NatToDataIdentifier => i
    }).asInstanceOf[T]
  }
}
