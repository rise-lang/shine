package rise.core

import arithexpr.arithmetic.ArithExpr

package object types {
  type Nat = ArithExpr

  type ->[T1 <: Type, T2 <: Type] = FunType[T1, T2]

  type `(dt)->`[T <: Type] = DataDepFunType[T]
  type DataDepFunType[T <: Type] = DepFunType[DataType, DataTypeIdentifier, T]

  type `(nat)->`[T <: Type] = NatDepFunType[T]
  type NatDepFunType[T <: Type] = DepFunType[Nat, NatIdentifier, T]

  type `(nat->nat)->`[T <: Type] = NatToNatDepFunType[T]
  type NatToNatDepFunType[T <: Type] = DepFunType[NatToNat, NatToNatIdentifier, T]

  type `(nat->data)->`[T <: Type] = NatToDataDepFunType[T]
  type NatToDataDepFunType[T <: Type] = DepFunType[NatToData, NatToDataIdentifier, T]
}
