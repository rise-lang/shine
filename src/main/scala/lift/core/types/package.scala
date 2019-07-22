package lift.core

package object types {

  type DataDepFunType[T <: Type] = DepFunType[DataKind, T]
  type NatDepFunType[T <: Type] = DepFunType[NatKind, T]
  type NatToNatDepFunType[T <: Type] = DepFunType[NatToNatKind, T]
  type NatToDataDepFunType[T <: Type] = DepFunType[NatToDataKind, T]

  type ->[T1 <: Type, T2 <: Type] = FunType[T1, T2]
  type `(nat)->`[T <: Type] = NatDepFunType[T]
  type `(dt)->`[T <: Type] = DataDepFunType[T]
  type `(nat->nat)->`[T <: Type] = NatToNatDepFunType[T]
  type `(nat->data)->`[T <: Type] = NatToDataDepFunType[T]

//  object TypeDependentFunctionType {
//    def apply[T <: Type](dt: DataTypeIdentifier, t: T): DependentFunctionType[DataKind, T] =
//      DependentFunctionType[DataKind, T](dt, t)
//  }
//
//  object NatDependentFunctionType {
//    def apply[T <: Type](n: NatIdentifier, t: T): DependentFunctionType[NatKind, T] =
//      DependentFunctionType[NatKind, T](n, t)
//  }
//
//  object AddressSpaceDependentFunctionType {
//    def apply[T <: Type](a: AddressSpaceIdentifier, t: T): DependentFunctionType[AddressSpaceKind, T] =
//      DependentFunctionType[AddressSpaceKind, T](a, t)
//  }

  implicit class DataTypeExtension(val dt: DataType) {
    def __(w: AccessType): DataAccessType = DataAccessType(dt, w)
    def _W: DataAccessType = DataAccessType(dt, lift.core.types.W)
    def _R: DataAccessType = DataAccessType(dt, lift.core.types.R)
  }
}
