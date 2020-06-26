package shine.DPIA.DSL

import rise.core.{types => rt}
import shine.DPIA.NatIdentifier
import shine.DPIA.Types._
import shine.DPIA.fromRise

object TypeLevel {

  implicit final class FunTypeConstructors(private val r: PhraseType)
    extends AnyVal {
    @inline def -->:(t: PhraseType): FunType[PhraseType, PhraseType] =
      FunType(t, r)
  }

  def exp(dt: DataType, a: AccessType): ExpType = ExpType(dt, a)
  def exp(dt: rt.DataType, a: AccessType): ExpType =
    exp(fromRise.dataType(dt), a)

  def acc(dt: DataType): AccType = AccType(dt)
  def acc(dt: rt.DataType): AccType = AccType(fromRise.dataType(dt))

  object nFunT {
    def apply(n: rt.NatIdentifier, t: PhraseType): PhraseType = {
      DepFunType[NatKind, PhraseType](fromRise.natIdentifier(n), t)
    }

    def apply(n: NatIdentifier, t: PhraseType): PhraseType = {
      DepFunType[NatKind, PhraseType](n, t)
    }

    def unapply[K <: Kind, T <: PhraseType](funType: DepFunType[K, T]
                                           ): Option[(NatIdentifier, T)] = {
      funType.x match {
        case n: NatIdentifier => Some((n, funType.t))
        case _ => throw new Exception("Expected Nat DepFunType")
      }
    }
  }

  object aFunT {
    def apply(a: rt.AddressSpaceIdentifier, t: PhraseType): PhraseType = {
      DepFunType[AddressSpaceKind, PhraseType](
        fromRise.addressSpaceIdentifier(a), t)
    }

    def apply(a: AddressSpaceIdentifier, t: PhraseType): PhraseType = {
      DepFunType[AddressSpaceKind, PhraseType](a, t)
    }

    def unapply[K <: Kind,
                T <: PhraseType](funType: DepFunType[K, T]
                                ): Option[(AddressSpaceIdentifier, T)] = {
      funType.x match {
        case a: AddressSpaceIdentifier => Some((a, funType.t))
        case _ => throw new Exception("Expected AddressSpace DepFunType")
      }
    }
  }

}
