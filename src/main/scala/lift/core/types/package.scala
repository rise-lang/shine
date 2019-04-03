package lift.core

package object types {
  type ->[T1 <: Type, T2 <: Type] = FunctionType[T1, T2]
  type `(nat)->`[T <: Type] = NatDependentFunctionType[T]
  type `(dt)->`[T <: Type] = TypeDependentFunctionType[T]

  type NatDependentDataType = NatDependentFunctionType[DataType]
  type `(nat)->dt` = NatDependentDataType
}
