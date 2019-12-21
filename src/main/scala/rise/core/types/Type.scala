package rise.core.types

import rise.core._
import arithexpr.arithmetic.RangeAdd

sealed trait Type

case class TypeException(msg: String) extends Exception {
  override def toString = s"type exception: $msg"
}

object TypePlaceholder extends Type {
  override def toString = "?"
}

final case class TypeIdentifier(name: String) extends Type with Kind.Identifier {
  override def toString: String = "_" + name
}

final case class FunType[T1 <: Type, T2 <: Type](inT: T1, outT: T2) extends Type {
  override def toString: String = s"($inT -> $outT)"
}

final case class DepFunType[K <: Kind : KindName, T <: Type](x: K#I with Kind.Explicitness, t: T) extends Type {
  override def toString: String = s"(${x.name}: ${implicitly[KindName[K]].get} -> $t)"

  override def equals(obj: Any): Boolean = obj match {
    case other: DepFunType[K, _] => t == lifting.liftDependentFunctionType[K](other)(x)
    case _ => false
  }
}

sealed trait DataType extends Type

final case class DataTypeIdentifier(name: String, override val isExplicit: Boolean = false)
  extends DataType with Kind.Identifier with Kind.Explicitness {
  override def toString: String = if (isExplicit) name else "_" + name
  override def asExplicit: DataTypeIdentifier = this.copy(isExplicit = true)
  override def asImplicit: DataTypeIdentifier = this.copy(isExplicit = false)
  override def equals(that: Any): Boolean = that match {
    case d: DataTypeIdentifier => this.name == d.name
    case _ => false
  }
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

final case class PairType(p1: DataType, p2: DataType) extends ComposedType {
  override def toString: String = s"($p1, $p2)"
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
  override def toString: String = s"idx[$size]"
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