package rise.core

import arithexpr.arithmetic.ArithExpr

package object types {
  type Nat = ArithExpr

  type ->[T1 <: Type, T2 <: Type] = FunType[T1, T2]

  type `(dt)->`[T <: Type, KI <: Kind.Identifier] = DataDepFunType[T, KI]
  type DataDepFunType[T <: Type, KI <: Kind.Identifier] = DepFunType[DataType, DataTypeIdentifier, KI, T]

  type `(nat)->`[T <: Type, KI <: Kind.Identifier] = NatDepFunType[T, KI]
  type NatDepFunType[T <: Type, KI <: Kind.Identifier] = DepFunType[Nat, NatIdentifier, KI, T]

  type `(nat->nat)->`[T <: Type, KI <: Kind.Identifier] = NatToNatDepFunType[T, KI]
  type NatToNatDepFunType[T <: Type, KI <: Kind.Identifier] = DepFunType[NatToNat, NatToNatIdentifier, KI, T]

  type `(nat->data)->`[T <: Type, KI <: Kind.Identifier] = NatToDataDepFunType[T, KI]
  type NatToDataDepFunType[T <: Type, KI <: Kind.Identifier] = DepFunType[NatToData, NatToDataIdentifier, KI, T]
}
