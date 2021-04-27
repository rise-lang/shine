package shine.DPIA

import arithexpr.arithmetic.RangeAdd
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.TypeCheck._


package object Types {
  implicit class ReverseInferenceHelper(pt: PhraseType) {
    def ::[T <: PhraseType](p: Phrase[T]): Unit =
      if (!(p checkTypeEqOrSubtype pt)) {
//        throw new Exception(s"Type error: found ${p.t}, expected $pt")
      }
    def `:`[T <: PhraseType](p: Phrase[T]): Unit =
      if (!(p checkTypeEqOrSubtype pt)) {
//        throw new Exception(s"Type error: found ${p.t}, expected $pt")
      }
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

  object n2dtFun {
    def apply(f: NatIdentifier => DataType): NatToDataLambda = {
      val x = NatIdentifier(freshName("n"))
      NatToDataLambda(x, f(x))
    }

    def apply(r: arithexpr.arithmetic.Range)
             (f: NatIdentifier => DataType): NatToDataLambda = {
      val x = NatIdentifier(freshName("n"), r)
      NatToDataLambda(x, f(x))
    }

    def apply(upperBound: Nat)
             (f: NatIdentifier => DataType): NatToDataLambda = {
      apply(RangeAdd(0, upperBound, 1))(f)
    }
  }
}
