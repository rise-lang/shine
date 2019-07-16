package lift.core

package object types {

  type TypeDependentFunctionType[T <: Type] = DependentFunctionType[DataKind, T]
  type NatDependentFunctionType[T <: Type] = DependentFunctionType[NatKind, T]
//  type NatDependentDataType = NatDependentFunctionType[DataType]

  type ->[T1 <: Type, T2 <: Type] = FunctionType[T1, T2]
  type `(nat)->`[T <: Type] = NatDependentFunctionType[T]
//  type `(nat)->dt` = NatDependentDataType
  type `(dt)->`[T <: Type] = TypeDependentFunctionType[T]
  type `(nat->nat)->`[T <: Type] = NatNatDependentFunctionType[T]
  type `(nat->data)->`[T <: Type] = NatDataTypeDependentFunctionType[T]

  object TypeDependentFunctionType {
    def apply[T <: Type](dt: DataTypeIdentifier, t: T): DependentFunctionType[DataKind, T] =
      DependentFunctionType[DataKind, T](dt, t)
  }

  object NatDependentFunctionType {
    def apply[T <: Type](n: NatIdentifier, t: T): DependentFunctionType[NatKind, T] =
      DependentFunctionType[NatKind, T](n, t)
  }

  object AddressSpaceDependentFunctionType {
    def apply[T <: Type](a: AddressSpaceIdentifier, t: T): DependentFunctionType[AddressSpaceKind, T] =
      DependentFunctionType[AddressSpaceKind, T](a, t)
  }
}
