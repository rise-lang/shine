package lift.core.types

import lift.core._
import lift.arithmetic._

sealed trait Type

final case class TypeIdentifier(name: String) extends Type {
  override def toString: String = name
}

final case class FunctionType[T1 <: Type, T2 <: Type](inT: T1, outT: T2) extends Type {
  override def toString: String = s"($inT -> $outT)"
}

final case class DependentFunctionType[K <: Kind, T <: Type](x: K#I, t: T)
                                                            (implicit kn: KindName[K]) extends Type {
  override def toString: String = s"(${x.name} : ${kn.get} -> $t)"
}

final case class NatNatDependentFunctionType[T <: Type](fn: NatNatFunctionIdentifier, t: T) extends Type {
  override def toString: String = s"(${fn.name}:nat -> nat -> $t)"
}

final case class NatDataTypeDependentFunctionType[T <: Type](fn: NatDataTypeFunctionIdentifier, t: T) extends Type {
  override def toString: String = s"(${fn.name}:nat -> data -> $t)"
}

case class DataAccessType(dt: DataType, w: AccessType) extends Type {
  override def toString: String = s"${dt}_$w"
}

// ============================================================================================= //
// Nat -> Nat
// ============================================================================================= //
sealed trait NatNatFunction {
  def apply(l: Nat): Nat
}

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

final case class NatNatFunctionIdentifier(name: String) extends NatNatFunction {
  override def apply(l: Nat): Nat = ???
}


// ============================================================================================= //
// Nat -> DataType
// ============================================================================================= //
sealed trait NatDataTypeFunction {
  def map(f:DataType => DataType):NatDataTypeFunction = {
    NatDataTypeFunction.mapOnElement(f, this)
  }

  def apply(n: Nat): NatDataTypeApply = call(n)
  def call(n: Nat): NatDataTypeApply = NatDataTypeApply(this, n)
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

  override def toString: String = s"($n: nat -> $dt)"

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

object NatDataTypeLambda {
  def apply(upperBound:Nat, f:NatIdentifier => DataType):NatDataTypeFunction = {
    val x = NatIdentifier(freshName("n"), RangeAdd(0, upperBound, 1))
    NatDataTypeLambda(x, f(x))
  }

  def apply(upperBound:Nat, id:NatIdentifier, body: DataType):NatDataTypeFunction = {
    val x = NamedVar(freshName("n"), RangeAdd(0, upperBound, 1))
    NatDataTypeLambda(x, substitute(_, `for`=id, `in`=body))
  }
}

final case class NatDataTypeFunctionIdentifier(name: String) extends NatDataTypeFunction {
  override def toString: String = name
}