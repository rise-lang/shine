package lift.core.types

import lift.core._
import lift.arithmetic._

sealed trait Type

// ============================================================================================= //
// (Function) Types
// ============================================================================================= //
final case class FunctionType[T1 <: Type, T2 <: Type](inT: T1, outT: T2) extends Type

final case class TypeDependentFunctionType[T <: Type](dt: DataTypeIdentifier, t: T) extends Type

final case class NatDependentFunctionType[T <: Type](n: NatIdentifier, t: T) extends Type

final case class NatNatDependentFunctionType[T <: Type](fn: NatNatFunctionIdentifier, t: T) extends Type

final case class NatDataTypeDependentFunctionType[T <: Type](fn: NatDataTypeFunctionIdentifier, t: T) extends Type


// ============================================================================================= //
// Data Types
// ============================================================================================= //
sealed trait DataType extends Type

final case class DataTypeIdentifier(name: String) extends DataType {
  override def toString: String = name
}

sealed trait ComposedType extends DataType

final case class ArrayType(size: Nat, elemType: DataType) extends ComposedType {
  override def toString: String = s"$size.$elemType"
}

final case class DepArrayType(size: Nat, fdt: NatDataTypeFunction) extends ComposedType {
  override def toString: String = s"$size.$fdt"
}

object DepArrayType {
  def apply(size: Nat, f: Nat => DataType): DepArrayType = {
   val newN = NamedVar(freshName("n"), RangeAdd(0, size, 1))
    val fdt = NatDataTypeLambda(newN, f(newN))
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


final case class NatDataTypeApply(f: NatDataTypeFunction, n: Nat) extends DataType {
  override def toString: String = s"$f($n)"
}

// ============================================================================================= //
// Nat -> Nat
// ============================================================================================= //
sealed trait NatNatFunction

final case class NatNatLambda private (n: NatIdentifier, m: Nat) extends NatNatFunction {
  //NatNatTypeFunction have an interesting comparison behavior, as we do not define
  //equality for them as simple syntactic equality: we just want to make sure their bodies
  //are equal up-to renaming of the binder.

  //However, just updating equals is not sufficient, as many data structures, such as HashMaps,
  //use hashCodes as proxy for equality. In order to make sure this property is respected, we ignore
  //the identifier variable, and just take the hash of the body evaluated at a known point
  override def hashCode(): Int = this(NamedVar("comparisonDummy")).hashCode()

  def apply(l: Nat): Nat = ArithExpr.substitute(m, Map((n, l)))

  override def toString: String = s"($n: nat) -> $m"

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: NatNatLambda => m == other(n)
      case _ => false
    }
  }
}

final case class NatNatFunctionIdentifier(name: String) extends NatNatFunction


// ============================================================================================= //
// Nat -> DataType
// ============================================================================================= //
sealed trait NatDataTypeFunction {
  def map(f:DataType => DataType):NatDataTypeFunction = {
    NatDataTypeFunction.mapOnElement(f, this)
  }
}

object NatDataTypeFunction {
  def mapOnElement(f:DataType => DataType,
          typeFun:NatDataTypeFunction):NatDataTypeFunction = {
    typeFun match {
      case ident:NatDataTypeFunctionIdentifier => ident
      case NatDataTypeLambda(binder, body) => NatDataTypeLambda(binder, f(body))
    }
  }
}

final case class NatDataTypeLambda(n: NatIdentifier, dt: DataType) extends NatDataTypeFunction {
//  //See hash code of NatNatTypeFunction
//  override def hashCode(): Int = this(NamedVar("ComparisonDummy")).hashCode()
//
//  def apply(m: Nat): DataType = ??? //DataType.substitute(m, `for`=n, `in`=body)

  override def toString: String = s"($n: nat) -> $dt"

//  override def equals(obj: Any): Boolean = {
//    obj match {
//      case other: NatDataTypeLambda =>
//        val subbedOther = other(n)
//        val eq = dt == subbedOther
//        eq
//      case _ => false
//    }
//  }
}

final case class NatDataTypeFunctionIdentifier(name: String) extends NatDataTypeFunction