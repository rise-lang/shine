package idealised

import idealised.DPIA.Phrases._
import idealised.DPIA.Types.{PhraseTypeParser, _}
import lift.arithmetic._
import lift.core

import scala.language.{implicitConversions, reflectiveCalls}

package object DPIA {

  def error(found: String, expected: String): Nothing = {
    throw new TypeException(s"Found $found but expected $expected")
  }

  def error(msg: String): Nothing = {
    throw new Exception(msg)
  }

  type Nat = ArithExpr
  type NatIdentifier = NamedVar with Kind.Identifier

  object NatIdentifier {
    def apply(name: String): NatIdentifier = new NamedVar(name) with Kind.Identifier
    def apply(name: String, range: Range): NatIdentifier = new NamedVar(name, range) with Kind.Identifier
  }

  implicit def surfaceToDPINatIdentifier(n: SurfaceLanguage.NatIdentifier): NatIdentifier = NatIdentifier(n.name, n.range)
  implicit def liftToDPIANatIdentifer(n: lift.core.NatIdentifier): NatIdentifier = NatIdentifier(n.name, n.range)

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

  // note: this is an easy fix to avoid name conflicts between lift and dpia
  val freshName: core.freshName.type = lift.core.freshName

  type x[T1 <: PhraseType, T2 <: PhraseType] = PairType[T1, T2]
  type ->[T1 <: PhraseType, T2 <: PhraseType] = FunType[T1, T2]
  type `->p`[T1 <: PhraseType, T2 <: PhraseType] = PassiveFunType[T1, T2]
  type `()->`[K <: Kind, T <: PhraseType] = DepFunType[K, T]
  type `(nat)->`[T <: PhraseType] = DepFunType[NatKind, T]
  type `(dt)->`[T <: PhraseType] = DepFunType[DataKind, T]
  type VarType = ExpType x AccType

  object VarType {
    def apply(dt: DataType): PairType[ExpType, AccType] = ExpType(dt) x AccType(dt)
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
    def x[T2 <: PhraseType](t2: T2) = PairType(t1, t2)
  }

  implicit class FunTypeConstructor[T1 <: PhraseType](t1: T1) {
    def ->[T2 <: PhraseType](t2: T2) = FunType(t1, t2)
  }

  implicit class PassiveFunTypeConstructor[T1 <: PhraseType](t1: T1) {
    def `->p`[T2 <: PhraseType](t2: T2) = PassiveFunType(t1, t2)
  }

  implicit final class NatDepFunTypeConstructor(private val x: NatIdentifier) {
    def `()->`[T <: PhraseType](t: T): `()->`[NatKind, T] = DepFunType[NatKind, T](x, t)
  }

  implicit final class LiftNatDepFunTypeConstructor(private val x: lift.core.NatIdentifier) {
    def `()->`[T <: PhraseType](t: T): `()->`[NatKind, T] = NatIdentifier(x.name, x.range) `()->` t
  }

  implicit class DataDepFunTypeConstructor(x: DataTypeIdentifier) {
    def `()->`[T <: PhraseType](outT: T): `()->`[DataKind, T] = DepFunType[DataKind, T](x, outT)
  }

  implicit class PhraseTypeHelper(val sc: StringContext) extends AnyVal {
    def t(args: Any*): PhraseType = {
      new PhraseTypeParser(sc.s(args:_*), sc.parts, args.iterator).parsePhraseType
    }

    def exp(args: Any*): ExpType = {
      new PhraseTypeParser("exp" + sc.s(args:_*), sc.parts, args.iterator).parseExpType
    }

    def acc(args: Any*): AccType = {
      new PhraseTypeParser("acc" + sc.s(args:_*), sc.parts, args.iterator).parseAccType
    }

    def varT(args: Any*): VarType = {
      new PhraseTypeParser("var" + sc.s(args:_*), sc.parts, args.iterator).parseVarType
    }

    def dt(args: Any*): DataType = {
      new PhraseTypeParser(sc.s(args:_*), sc.parts, args.iterator).parseWrappedDataType
    }
  }

}
