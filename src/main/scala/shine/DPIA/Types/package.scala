package shine.DPIA

import arithexpr.arithmetic.RangeAdd
import rise.core.types.{AccessIdentifier, AccessKind, AddressSpaceIdentifier, AddressSpaceKind, DataKind, DataType, DataTypeIdentifier, NatIdentifier, NatKind, NatToDataLambda}
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.TypeCheck._

import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION


package object Types {
  @elidable(ASSERTION)
  def typeAssert[T <: PhraseType](p: Phrase[T], pt: PhraseType): Unit = {
    if (!(p checkTypeEqOrSubtype pt))
      throw new java.lang.AssertionError(s"Type error: found ${p.t}, expected $pt")
  }

  @elidable(ASSERTION)
  def typeAssert(boolean: Boolean, msg: => String): Unit = {
    if (!boolean)
      throw new java.lang.AssertionError(s"Type error: $msg")
  }

  implicit class ReverseInferenceHelper(pt: PhraseType) {
    def ::[T <: PhraseType](p: Phrase[T]): Unit = typeAssert(p, pt)
    def `:`[T <: PhraseType](p: Phrase[T]): Unit = typeAssert(p, pt)
  }

  type NatDependentFunctionType[T <: PhraseType] = `(nat)->:`[T]

  object NatDependentFunctionType {
    def apply[T <: PhraseType](n: NatIdentifier, t: T): `(nat)->:`[T] =
      DepFunType(NatKind, n, t)
  }

  type TypeDependentFunctionType[T <: PhraseType] = `(dt)->:`[T]

  object TypeDependentFunctionType {
    def apply[T <: PhraseType](dt: DataTypeIdentifier, t: T): `(dt)->:`[T] =
      DepFunType(DataKind, dt, t)
  }

  type AddrSpaceDependentFunctionType[T <: PhraseType] = `(add)->:`[T]

  object AddrSpaceDependentFunctionType {
    def apply[T <: PhraseType](addr: AddressSpaceIdentifier, t: T): `(add)->:`[T] =
      DepFunType(AddressSpaceKind, addr, t)
  }

  type AccessDependentFunctionType[T <: PhraseType] = `(acc)->:`[T]

  object AccessDependentFunctionType {
    def apply[T <: PhraseType](at: AccessIdentifier, t: T): `(acc)->:`[T] =
      DepFunType(AccessKind, at, t)
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
