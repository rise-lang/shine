package lift.core.types

import lift.core._
import lift.arithmetic._

sealed trait Type

// ============================================================================================= //
// (Function) Types
// ============================================================================================= //
final case class FunType[T1 <: Type, T2 <: Type](inT: T1, outT: T2) extends Type {
  override def toString: String = s"($inT -> $outT)"
}

final case class DepFunType[K <: Kind, T <: Type](x: K#I, t: T) extends Type {
  override def toString: String =
    s"(${x.name}: ${x.getClass.getName.dropWhile(_!='$').drop(1).takeWhile(_!='$')} -> $t)"
}

// ============================================================================================= //
// Data Types
// ============================================================================================= //
sealed trait DataType extends Type

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
   val newN = NatIdentifier(freshName("n"), RangeAdd(0, size, 1))
    val fdt = NatToDataLambda(newN, f(newN))
    DepArrayType(size, fdt)
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
sealed case class VectorType(size: Nat, elemType: Type) extends BasicType {
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

// ============================================================================================= //
// Nat -> Nat
// ============================================================================================= //
sealed trait NatToNat {
  def apply(n: Nat): Nat = NatToNatApply(this, n)
}

final case class NatToNatLambda private(x: NatIdentifier, body: Nat) extends NatToNat {
  // NatToNat have an interesting comparison behavior, as we do not define
  // equality for them as simple syntactic equality: we just want to make sure their bodies
  // are equal up-to renaming of the binder.

  // However, just updating equals is not sufficient, as many data structures, such as HashMaps,
  // use hashCodes as proxy for equality. In order to make sure this property is respected, we ignore
  // the identifier variable, and just take the hash of the body evaluated at a known point
  override def hashCode(): Int = this(NamedVar("comparisonDummy")).hashCode()

  override def apply(l: Nat): Nat = ArithExpr.substitute(body, Map((x, l)))

  override def toString: String = s"($x: nat |-> $body)"

  override def equals(obj: Any): Boolean = obj match {
    case other: NatToNatLambda => body == other(x)
    case _ => false
  }
}

object NatToNatLambda {
  def apply(upperBound:Nat, f: NatIdentifier => Nat): NatToNatLambda = {
    val x = NatIdentifier(freshName("n"), RangeAdd(0, upperBound, 1))
    NatToNatLambda(x, f(x))
  }

  def apply(upperBound:Nat, id: NatIdentifier, body:Nat): NatToNatLambda = {
    val x = NamedVar(freshName("n"), RangeAdd(0, upperBound, 1))
    NatToNatLambda(x, x => ArithExpr.substitute(body, Map((id, x))))
  }
}

final case class NatToNatIdentifier(name: String) extends NatToNat with Kind.Identifier

final class NatToNatApply(val f: NatToNat, val n: Nat) extends ArithExprFunction(s"$f($n)") {
  override def visitAndRebuild(f: Nat => Nat): Nat = this
}
object NatToNatApply {
  def apply(f: NatToNat, n: Nat): Nat = f match {
    case l: NatToNatLambda      => l.apply(n)
    case i: NatToNatIdentifier  => new NatToNatApply(i, n)
  }
  def unapply(arg: NatToNatApply): Option[(NatToNat, Nat)] = Some((arg.f, arg.n))
}


// ============================================================================================= //
// Nat -> DataType
// ============================================================================================= //
sealed trait NatToData {
  def map(f: DataType => DataType): NatToData = {
    NatToData.mapOnElement(f, typeFun = this)
  }

  def apply(n: Nat): DataType = NatToDataApply(this, n)
}

object NatToData {
  def mapOnElement(f:DataType => DataType, typeFun: NatToData): NatToData = typeFun match {
    case ident:NatToDataIdentifier => ident
    case NatToDataLambda(binder, body) => NatToDataLambda(binder, f(body))
  }
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

object NatToDataLambda {
  def apply(upperBound: Nat, f: NatIdentifier => DataType): NatToData = {
    val n = NatIdentifier(freshName("n"), RangeAdd(0, upperBound, 1))
    NatToDataLambda(n, f(n))
  }

  def apply(upperBound: Nat, n: NatIdentifier, body: DataType): NatToData = {
    val n = NamedVar(freshName("n"), RangeAdd(0, upperBound, 1))
    NatToDataLambda(n, substitute(_, `for`=n, `in`=body))
  }
}

final case class NatToDataIdentifier(name: String) extends NatToData with Kind.Identifier {
  override def toString: String = name
}