package lift.core

package object types {

  type DataDepFunType[T <: Type] = DepFunType[DataKind, T]
  type NatDepFunType[T <: Type] = DepFunType[NatKind, T]
  type NatToNatDepFunType[T <: Type] = DepFunType[NatToNatKind, T]
  type NatToDataDepFunType[T <: Type] = DepFunType[NatToDataKind, T]
  type NatDepDataType = NatDepFunType[DataType]

  type ->[T1 <: Type, T2 <: Type] = FunType[T1, T2]
  type `(nat)->`[T <: Type] = NatDepFunType[T]
  type `(nat)->dt` = NatDepDataType
  type `(dt)->`[T <: Type] = DataDepFunType[T]
  type `(nat->nat)->`[T <: Type] = NatToNatDepFunType[T]
  type `(nat->data)->`[T <: Type] = NatToDataDepFunType[T]

  object DataDepFunType {
    def apply[T <: Type](dt: DataTypeIdentifier, t: T): DepFunType[DataKind, T] =
      DepFunType[DataKind, T](dt, t)
  }

  object NatDepFunType {
    def apply[T <: Type](n: NatIdentifier, t: T): DepFunType[NatKind, T] =
      DepFunType[NatKind, T](n, t)
  }

  object NatToNatFunType {
    def apply[T <: Type](x: NatToNatIdentifier, t: T): DepFunType[NatToNatKind, T] =
      DepFunType[NatToNatKind, T](x, t)
  }

  object NatToDataFunType {
    def apply[T <: Type](x: NatToDataIdentifier, t: T): DepFunType[NatToDataKind, T] =
      DepFunType[NatToDataKind, T](x, t)
  }
}
