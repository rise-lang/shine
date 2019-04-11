package lift.core.types

import lift.core._
import lift.arithmetic._

sealed trait Type

// data types
sealed trait DataType extends Type

final case class DataTypeIdentifier(name: String) extends DataType


sealed trait ComposedType extends DataType

final case class ArrayType(size: Nat, elemType: DataType) extends ComposedType {
  override def toString: String = s"$size.$elemType"
}

final case class DepArrayType(size: Nat, elemType: `(nat)->dt`) extends ComposedType {
  override def toString: String = s"$size.${elemType.n} -> ${elemType.t}"
}

object DepArrayType {
  def apply(size: Nat, f: Nat => DataType): DepArrayType = {
    val newN = NamedVar(freshName("n"), RangeAdd(0, size, 1))
    DepArrayType(size, NatDependentFunctionType(newN, f(newN)))
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

final case class IndexType(size: Nat) extends BasicType


sealed case class VectorType(size: Nat, elemType: ScalarType) extends BasicType {
  override def toString: String = s"$elemType$size"
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


// function types
final case class FunctionType[T1 <: Type, T2 <: Type](inT: T1, outT: T2) extends Type {
  override def toString: String = s"($inT) -> $outT"
}

final case class TypeDependentFunctionType[T <: Type](dt: DataTypeIdentifier, t: T) extends Type {
  override def toString: String = s"($dt : data) -> $t"
}

final case class NatDependentFunctionType[T <: Type](n: NatIdentifier, t: T) extends Type {
  override def toString: String = s"($n : nat) -> $t"
}