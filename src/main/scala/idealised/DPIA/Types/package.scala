package idealised.DPIA

import idealised.DPIA.Types.TypeCheck._
import idealised.DPIA.Phrases.{Phrase, ToString}
import idealised.OpenCL.AddressSpace


package object Types {
  implicit class ReverseInferenceHelper(pt: PhraseType) {
    def ::[T <: PhraseType](p: Phrase[T]): Unit = p checkType pt
    def `:`[T <: PhraseType](p: Phrase[T]): Unit = p checkType pt
  }

  implicit class CheckHelper(p1: PhraseType) {
    def |(p2: PhraseType): PhraseType => Unit = (p: PhraseType) => {
      if (!(p == p1 || p == p2)) {
        error(ToString(p), expected = ToString(p1) + " or " + ToString(p2))
      }
    }
  }

  implicit final class ArrowDataType(private val self: DataType) {
    @inline def -> [B](y: B): B = y
    def →[B](y: B): B = ->(y)
  }

  implicit final class ArrowNat(private val self: Nat) {
    @inline def -> [B](y: B): B = y
    def →[B](y: B): B = ->(y)
  }

  implicit final class ArrowAddressSpace(private val self: AddressSpace) {
    @inline def -> [B](y: B): B = y
    def →[B](y: B): B = ->(y)
  }

  implicit final class ArrowAddrSpace(private val self: AddrSpace) {
    @inline def -> [B](y: B): B = y
    def →[B](y: B): B = ->(y)
  }

  implicit final class ArrowNatNatTypeFun(private val self: NatNatTypeFunction) {
    @inline def -> [B](y: B): B = y
    def →[B](y: B): B = ->(y)
  }

  implicit final class ArrowNatDataTypeFun(private val self: NatDataTypeFunction) {
    @inline def -> [B](y: B): B = y
    def →[B](y: B): B = ->(y)
  }

  implicit final class ArrowUnit(private val self: Unit) {
    @inline def -> [B](y: B): B = y
    def →[B](y: B): B = ->(y)
  }

  type NatDependentFunctionType[T <: PhraseType] = DependentFunctionType[NatKind, T]

  object NatDependentFunctionType {
    def apply[T <: PhraseType](n: NatIdentifier, t: T): DependentFunctionType[NatKind, T] =
      DependentFunctionType[NatKind, T](n, t)
  }

  type TypeDependentFunctionType[T <: PhraseType] = DependentFunctionType[DataKind, T]

  object TypeDependentFunctionType {
    def apply[T <: PhraseType](dt: DataTypeIdentifier, t: T): DependentFunctionType[DataKind, T] =
      DependentFunctionType[DataKind, T](dt, t)
  }

  type AddrSpaceDependentFunctionType[T <: PhraseType] = DependentFunctionType[AddrKind, T]

  object AddrSpaceDependentFunctionType {
    def apply[T <: PhraseType](addr: AddrSpaceIdentifier, t: T): DependentFunctionType[AddrKind, T] =
      DependentFunctionType[AddrKind, T](addr, t)
  }

  type AccessDependentFunctionType[T <: PhraseType] = DependentFunctionType[AccessKind, T]

  object AccessDependentFunctionType {
    def apply[T <: PhraseType](at: AccessTypeIdentifier, t: T): DependentFunctionType[AccessKind, T] =
      DependentFunctionType[AccessKind, T](at, t)
  }
}
