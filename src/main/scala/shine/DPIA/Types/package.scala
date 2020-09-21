package shine.DPIA

import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.TypeCheck._


package object Types {
  implicit class ReverseInferenceHelper(pt: PhraseType) {
    def ::[T <: PhraseType](p: Phrase[T]): Unit = p checkTypeEqOrSubtype pt
    def `:`[T <: PhraseType](p: Phrase[T]): Unit = p checkTypeEqOrSubtype pt
  }

  type NatDependentFunctionType[T <: PhraseType] = DepFunType[NatKind, T]

  object NatDependentFunctionType {
    def apply[T <: PhraseType](n: NatIdentifier, t: T): DepFunType[NatKind, T] =
      DepFunType[NatKind, T](n, t)
  }

  type TypeDependentFunctionType[T <: PhraseType] = DepFunType[DataKind, T]

  object TypeDependentFunctionType {
    def apply[T <: PhraseType](
      dt: DataTypeIdentifier,
      t: T
    ): DepFunType[DataKind, T] =
      DepFunType[DataKind, T](dt, t)
  }

  type AddrSpaceDependentFunctionType[T <: PhraseType] =
    DepFunType[AddressSpaceKind, T]

  object AddrSpaceDependentFunctionType {
    def apply[T <: PhraseType](
      addr: AddressSpaceIdentifier,
      t: T
    ): DepFunType[AddressSpaceKind, T] =
      DepFunType[AddressSpaceKind, T](addr, t)
  }

  type AccessDependentFunctionType[T <: PhraseType] = DepFunType[AccessKind, T]

  object AccessDependentFunctionType {
    def apply[T <: PhraseType](
      at: AccessTypeIdentifier,
      t: T
    ): DepFunType[AccessKind, T] =
      DepFunType[AccessKind, T](at, t)
  }
}
