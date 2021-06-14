package shine

import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import rise.core.{types => rt}

package object DPIA {

  def error(found: String, expected: String): Nothing = {
    throw new TypeException(s"Found $found but expected $expected")
  }

  def error(msg: String = "This should not happen"): Nothing = {
    throw new Exception(msg)
  }

  type Nat = ArithExpr
  type NatIdentifier = NamedVar with Kind.Identifier

  object Nat {
    def substitute[N <: Nat](ae: Nat, `for`: NatIdentifier, in: N): N =
      ArithExpr.substitute(in, Map(`for` -> ae)).asInstanceOf[N]
  }

  object NatIdentifier {
    def apply(name: String): NatIdentifier = new NamedVar(name) with Kind.Identifier
    def apply(name: String, range: Range): NatIdentifier = new NamedVar(name, range) with Kind.Identifier
  }

  // note: this is an easy fix to avoid name conflicts between lift and dpia
  val freshName: rise.core.freshName.type = rise.core.freshName

  type x[T1 <: PhraseType, T2 <: PhraseType] = PhrasePairType[T1, T2]
  type ->:[T <: PhraseType, R <: PhraseType] = FunType[T, R]
  type `->p:`[T <: PhraseType, R <: PhraseType] = PassiveFunType[T, R]
  type `()->:`[I <: Kind.Identifier, R <: PhraseType] = DepFunType[I, R]
  type `(nat)->:`[R <: PhraseType] = DepFunType[NatIdentifier, R]
  type `(dt)->:`[R <: PhraseType] = DepFunType[DataTypeIdentifier, R]
  type VarType = ExpType x AccType

  object VarType {
    def apply(dt: DataType): PhrasePairType[ExpType, AccType] = ExpType(dt, read) x AccType(dt)
  }

  //noinspection TypeAnnotation
  implicit class PhraseTypeSubstitutionHelper[T <: PhraseType](t: PhraseType) {
    def `[`(e: Nat) = new {
      def `/`(a: Nat) = new {
        def `]`: PhraseType = PhraseType.substitute(e, `for`=a, in=t)
      }
    }

    def `[`(e: DataType) = new {
      def `/`(a: DataType) = new {
        def `]`: PhraseType = PhraseType.substitute(e, `for`=a, in=t)
      }
    }
  }

  //noinspection TypeAnnotation
  implicit class PhraseSubstitutionHelper[T1 <: PhraseType](in: Phrase[T1]) {
    def `[`[T2 <: PhraseType](p: Phrase[T2]) = new {
      def `/`(`for`: Phrase[T2]) = new {
        def `]`: Phrase[T1] = Phrase.substitute(p, `for`, in)
      }
    }

    def `[`(e: Nat) = new {
      def `/`(`for`: NatIdentifier) = new {
        def `]`: Phrase[T1] = PhraseType.substitute(e, `for`, in)
      }
    }

    def `[`(dt: DataType) = new {
      def `/`(`for`: DataTypeIdentifier) = new {
        def `]`: Phrase[T1] = PhraseType.substitute(dt, `for`, in)
      }
    }
  }

  implicit class PairTypeConstructor[T1 <: PhraseType](t1: T1) {
    @inline
    def x[T2 <: PhraseType](t2: T2): T1 x T2 = PhrasePairType(t1, t2)
  }

  implicit class FunTypeConstructor[R <: PhraseType](r: R) {
    @inline
    def ->:[T <: PhraseType](t: T): T ->: R = FunType(t, r)
  }

  implicit class PassiveFunTypeConstructor[R <: PhraseType](r: R) {
    def `->p:`[T <: PhraseType](t: T): T `->p:` R = PassiveFunType(t, r)
  }

  implicit class DepFunTypeConstructor[R <: PhraseType](r: R) {
    def ->:(i: DataTypeIdentifier): `()->:`[DataTypeIdentifier, R] = DepFunType(DataKind, i, r)
    def ->:(n: NatIdentifier): `()->:`[NatIdentifier, R] = DepFunType(NatKind, n, r)
    def ->:(n: NatToNatIdentifier): `()->:`[NatToNatIdentifier, R] = DepFunType(NatToNatKind, n, r)
    def ->:(n: NatToDataIdentifier): `()->:`[NatToDataIdentifier, R] = DepFunType(NatToDataKind, n, r)
  }

  object expT {
    def apply(dt: DataType, a: AccessType): ExpType = ExpType(dt, a)
    def apply(dt: rt.DataType, a: AccessType): ExpType =
      expT(fromRise.dataType(dt), a)

    def unapply(et: ExpType): Option[(DataType, AccessType)] = {
      ExpType.unapply(et)
    }
  }

  object accT {
    def apply(dt: DataType): AccType = AccType(dt)
    def apply(dt: rt.DataType): AccType = accT(fromRise.dataType(dt))

    def unapply(at: AccType): Option[DataType] = {
      AccType.unapply(at)
    }
  }

  object varT {
    def apply(dt: DataType): VarType = expT(dt, read) x accT(dt)
    def apply(dt: rt.DataType): VarType = expT(dt, read) x accT(dt)
  }

  object nFunT {
    def apply(n: NatIdentifier, t: PhraseType): PhraseType = {
      DepFunType(NatKind, n, t)
    }

    def unapply[I <: Kind.Identifier, U <: PhraseType](funType: DepFunType[I, U]): Option[(NatIdentifier, U)] = {
      funType.x match {
        case n: NatIdentifier => Some((n, funType.t))
        case _ => throw new Exception("Expected Nat DepFunType")
      }
    }
  }

  object dFunT {
    def apply(d: rt.DataTypeIdentifier, t: PhraseType): PhraseType = {
      DepFunType(DataKind, fromRise.dataTypeIdentifier(d), t)
    }
  }

  object aFunT {
    def apply(a: rt.AddressSpaceIdentifier, t: PhraseType): PhraseType = {
      DepFunType(AddressSpaceKind, fromRise.addressSpaceIdentifier(a), t)
    }

    def apply(a: AddressSpaceIdentifier, t: PhraseType): PhraseType = {
      DepFunType(AddressSpaceKind, a, t)
    }

    def unapply[I <: Kind.Identifier, T <: PhraseType](funType: DepFunType[I, T]
                                                      ): Option[(AddressSpaceIdentifier, T)] = {
      funType.x match {
        case a: AddressSpaceIdentifier => Some((a, funType.t))
        case _ => throw new Exception("Expected AddressSpace DepFunType")
      }
    }
  }

  object n2nFunT {
    def apply(n: rt.NatToNatIdentifier, t: PhraseType): PhraseType = {
      DepFunType(NatToNatKind, fromRise.natToNatIdentifier(n), t)
    }

    def apply(n: NatToNatIdentifier, t: PhraseType): PhraseType = {
      DepFunType(NatToNatKind, n, t)
    }

    def unapply[I <: Kind.Identifier, T <: PhraseType](funType: DepFunType[I, T]
                                                      ): Option[(NatToNatIdentifier, T)] = {
      funType.x match {
        case n: NatToNatIdentifier => Some((n, funType.t))
        case _ => throw new Exception("Expected Nat DepFunType")
      }
    }
  }
}
