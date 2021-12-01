package rise.core

import arithexpr.arithmetic.{ArithExpr, NamedVar}
import rise.core.types.DataType.DataTypeIdentifier

package object types {
  type NatIdentifier = NamedVar
  type Nat = ArithExpr

  type ->[T1 <: ExprType, T2 <: ExprType] = FunType[T1, T2]

  type `(dt)->`[T <: ExprType] = DataDepFunType[T]
  type DataDepFunType[T <: ExprType] = DepFunType[DataType, DataTypeIdentifier, T]

  type `(nat)->`[T <: ExprType] = NatDepFunType[T]
  type NatDepFunType[T <: ExprType] = DepFunType[Nat, NatIdentifier, T]

  type `(nat->nat)->`[T <: ExprType] = NatToNatDepFunType[T]
  type NatToNatDepFunType[T <: ExprType] = DepFunType[NatToNat, NatToNatIdentifier, T]

  type `(nat->data)->`[T <: ExprType] = NatToDataDepFunType[T]
  type NatToDataDepFunType[T <: ExprType] = DepFunType[NatToData, NatToDataIdentifier, T]
}
