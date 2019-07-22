package lift.core.types

import lift.arithmetic.RangeAdd
import lift.core.{Nat, NatIdentifier, freshName}


sealed trait DataType

final case class DataTypeIdentifier(name: String) extends DataType with Kind.Identifier {
  override def toString: String = name
}

sealed trait ComposedType extends DataType

final case class ArrayType(size: Nat, elemType: DataType) extends ComposedType {
  override def toString: String = s"$size.$elemType"
}

final case class DepArrayType(size: Nat, fdt: NatToData) extends ComposedType {
  override def toString: String = s"$size.$fdt"
}

object DepArrayType {
  def apply(size: Nat, f: Nat => DataType): DepArrayType = {
    val n = NatIdentifier(freshName("n"), RangeAdd(0, size, 1))
    DepArrayType(size, NatToDataLambda(n, f(n)))
  }
}

final case class TupleType(elemTypes: DataType*) extends ComposedType {
  assert(elemTypes.size == 2)

  override def toString: String = elemTypes.map(_.toString).mkString("(", ", ", ")")
}


sealed trait BasicType extends DataType


sealed trait ScalarType extends BasicType

object bool extends ScalarType {
  override def toString: String = "bool"
}

object int extends ScalarType {
  override def toString: String = "int"
}

object float extends ScalarType {
  override def toString: String = "float"
}

object double extends ScalarType { override def toString: String = "double" }

object NatType extends ScalarType { override def toString: String = "nat"}

final case class IndexType(size: Nat) extends BasicType {
  override def toString: String = s"idx($size)"
}

// TODO: enforce ScalarType
sealed case class VectorType(size: Nat, elemType: DataType) extends BasicType {
  override def toString: String = s"<$size>$elemType"
}

object int2 extends VectorType(2, int)

object int3 extends VectorType(3, int)

object int4 extends VectorType(4, int)

object int8 extends VectorType(8, int)

object int16 extends VectorType(16, int)

object float2 extends VectorType(2, float)

object float3 extends VectorType(3, float)

object float4 extends VectorType(4, float)

object float8 extends VectorType(8, float)

object float16 extends VectorType(16, float)


final class NatToDataApply(val f: NatToData, val n: Nat) extends DataType {
  override def toString: String = s"$f($n)"
}

object NatToDataApply {
  def apply(f: NatToData, n: Nat): DataType = f match {
    case l: NatToDataLambda     => l.apply(n)
    case i: NatToDataIdentifier => new NatToDataApply(i, n)
  }

  def unapply(arg: NatToDataApply): Option[(NatToData, Nat)] = Some((arg.f, arg.n))
}
