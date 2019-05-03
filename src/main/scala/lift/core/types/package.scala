package lift.core
package object types {
  type ->[T1 <: Type, T2 <: Type] = FunctionType[T1, T2]
  type `(nat)->`[T <: Type] = NatDependentFunctionType[T]
  type `(nat)->dt` = NatDependentDataType
  type `(dt)->`[T <: Type] = TypeDependentFunctionType[T]
  type `(nat->nat)->`[T <: Type] = NatNatDependentFunctionType[T]
  type `(nat->data)->`[T <: Type] = NatDataTypeDependentFunctionType[T]

  type NatDependentDataType = NatDependentFunctionType[DataType]
}
