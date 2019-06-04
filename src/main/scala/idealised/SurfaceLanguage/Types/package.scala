package idealised.SurfaceLanguage

package object Types {

  type TypeDependentFunctionType[T <: Type] = DependentFunctionType[DataKind, T]

  object NatDependentFunctionType {
    def apply[T <: Type](n: NatIdentifier, t: T): DependentFunctionType[NatKind, T] =
      DependentFunctionType[NatKind, T](n, t)
  }

  type NatDependentFunctionType[T <: Type] = DependentFunctionType[NatKind, T]

  object TypeDependentFunctionType {
    def apply[T <: Type](dt: DataTypeIdentifier, t: T): DependentFunctionType[DataKind, T] =
      DependentFunctionType[DataKind, T](dt, t)
  }
}
