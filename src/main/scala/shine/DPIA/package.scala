package shine

import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types._

package object DPIA {

  def error(found: String, expected: String): Nothing = {
    throw new TypeException(s"Found $found but expected $expected")
  }

  def error(msg: String): Nothing = {
    throw new Exception(msg)
  }

  type Nat = ArithExpr
  type NatIdentifier = NamedVar with Kind.Identifier

  object Nat {
    def substitute[N <: Nat](ae: Nat, `for`: NatIdentifier, in: N): N = {
      in.visitAndRebuild {
        case v: Var =>
          if (`for`.name == v.name) {
            ae
          } else {
            v
          }
        case e => e
      }.asInstanceOf[N]
    }
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
  type `()->:`[K <: Kind, R <: PhraseType] = DepFunType[K, R]
  type `(nat)->:`[R <: PhraseType] = DepFunType[NatKind, R]
  type `(dt)->:`[R <: PhraseType] = DepFunType[DataKind, R]
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
    def ->:(i: DataTypeIdentifier): `()->:`[DataKind, R] = DepFunType[DataKind, R](i, r)
    def ->:(n: NatIdentifier): `()->:`[NatKind, R] = DepFunType[NatKind, R](n, r)
    def ->:(n: NatToNatIdentifier): `()->:`[NatToNatKind, R] = DepFunType[NatToNatKind, R](n, r)
    def ->:(n: NatToDataIdentifier): `()->:`[NatToDataKind, R] = DepFunType[NatToDataKind, R](n, r)
  }

  @inline
  def expT(dt: DataType, a: AccessType): ExpType = ExpType(dt, a)
  @inline
  def accT(dt: DataType): AccType = AccType(dt)
  @inline
  def varT(dt: DataType): VarType = ExpType(dt, read) x AccType(dt)
}
