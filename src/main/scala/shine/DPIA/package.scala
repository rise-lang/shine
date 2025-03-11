package shine

import arithexpr.arithmetic._
import rise.core.types.DataType.DataTypeIdentifier
import rise.core.types.{DepFunType => _, FunType => _, ExprType => _, TypeIdentifier => _, TypePlaceholder => _, _}
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import rise.core.{types => rt}

package object DPIA {

  def error(found: String, expected: String): Nothing = {
    throw TypeException(s"Found $found but expected $expected")
  }

  def error(msg: String = "This should not happen"): Nothing = {
    throw new Exception(msg)
  }

  type Nat = ArithExpr
  type NatIdentifier = NamedVar

  object Nat {
    def substitute[N <: Nat](ae: Nat, `for`: NatIdentifier, in: N): N =
      ArithExpr.substitute(in, Map(`for` -> ae)).asInstanceOf[N]
  }

//  object NatIdentifier {
//    def apply(name: String): NatIdentifier = NamedVar(name)
//    def apply(name: String, range: Range): NatIdentifier = NamedVar(name, range)
//  }

  // note: this is an easy fix to avoid name conflicts between lift and dpia
  val freshName: rise.core.freshName.type = rise.core.freshName

  type x[T1 <: PhraseType, T2 <: PhraseType] = PhrasePairType[T1, T2]
  type ->:[T <: PhraseType, R <: PhraseType] = FunType[T, R]
  type `->p:`[T <: PhraseType, R <: PhraseType] = PassiveFunType[T, R]
  type `(nat)->:`[R <: PhraseType] = DepFunType[NatIdentifier, R]
  type `(dt)->:`[R <: PhraseType] = DepFunType[DataTypeIdentifier, R]
  type `(add)->:`[R <: PhraseType] = DepFunType[AddressSpaceIdentifier, R]
  type `(acc)->:`[R <: PhraseType] = DepFunType[AccessIdentifier, R]
  type `(n2n)->:`[R <: PhraseType] = DepFunType[NatToNatIdentifier, R]
  type `(n2d)->:`[R <: PhraseType] = DepFunType[NatToDataIdentifier, R]
  type VarType = ExpType x AccType

  object VarType {
    def apply(dt: DataType): PhrasePairType[ExpType, AccType] = ExpType(dt, read) x AccType(dt)
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
    def ->:(i: DataTypeIdentifier): DepFunType[DataTypeIdentifier, R] = DepFunType(DataKind, i, r)
    def ->:(n: NatIdentifier): DepFunType[NatIdentifier, R] = DepFunType(NatKind, n, r)
    def ->:(n: NatToNatIdentifier): DepFunType[NatToNatIdentifier, R] = DepFunType(NatToNatKind, n, r)
    def ->:(n: NatToDataIdentifier): DepFunType[NatToDataIdentifier, R] = DepFunType(NatToDataKind, n, r)
  }

  object expT {
    def apply(dt: DataType, a: Access): ExpType = ExpType(dt, a)

    def unapply(et: ExpType): Option[(DataType, Access)] = {
      Some((et.dataType, et.accessType))
    }
  }

  object accT {
    def apply(dt: DataType): AccType = AccType(dt)

    def unapply(at: AccType): Option[DataType] = {
      Some(at.dataType)
    }
  }

  object varT {
    def apply(dt: DataType): VarType = expT(dt, read) x accT(dt)
  }

  object nFunT {
    def apply(n: NatIdentifier, t: PhraseType): PhraseType = {
      DepFunType(NatKind, n, t)
    }

    // FIXME: does this work?
    def unapply/*[U <: PhraseType]*/(funType: DepFunType[_, _/*U*/]): Option[(NatIdentifier, Any)] = {
      funType.x match {
        case n: NatIdentifier => Some((n, funType.t)) // .asInstanceOf[U]
        case _ => throw new Exception("Expected Nat DepFunType")
      }
    }
  }

  object dFunT {
    def apply(d: DataTypeIdentifier, t: PhraseType): PhraseType = {
      DepFunType(DataKind, d, t)
    }
  }

  object aFunT {
    def apply(a: AddressSpaceIdentifier, t: PhraseType): PhraseType = {
      DepFunType(AddressSpaceKind, a, t)
    }

    // FIXME: does this work?
    def unapply[T <: PhraseType](funType: DepFunType[_, _/*T*/]): Option[(AddressSpaceIdentifier, Any)] = {
      funType.x match {
        case a: AddressSpaceIdentifier => Some((a, funType.t)) // .asInstanceOf[T]
        case _ => throw new Exception("Expected AddressSpace DepFunType")
      }
    }
  }

  object n2nFunT {
    def apply(n: NatToNatIdentifier, t: PhraseType): PhraseType = {
      DepFunType(NatToNatKind, n, t)
    }

    // FIXME: does this work?
    def unapply/*[T <: PhraseType]*/(funType: DepFunType[_, _/*T*/]): Option[(NatToNatIdentifier, Any)] = {
      funType.x match {
        case n: NatToNatIdentifier => Some((n, funType.t)) // .asInstanceOf[T]
        case _ => throw new Exception("Expected Nat DepFunType")
      }
    }
  }
}
