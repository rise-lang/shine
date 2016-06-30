import Core.PhraseType._
import apart.arithmetic.{ArithExpr, NamedVar}

import scala.language.implicitConversions
import scala.language.reflectiveCalls

package object Core {

  type x[T1 <: PhraseType, T2 <: PhraseType] = PairType[T1, T2]
  type ->[T1 <: PhraseType, T2 <: PhraseType] = FunctionType[T1, T2]
  type `->p`[T1 <: PhraseType, T2 <: PhraseType] = PassiveFunctionType[T1, T2]
  type `(nat)->`[T <: PhraseType] = NatDependentFunctionType[T]
  type VarType = ExpType x AccType

  object VarType {
    def apply(dt: DataType) = ExpType(dt) x AccType(dt)
  }

  implicit class SubstituionHelper[T <: PhraseType](t: PhraseType) {
    def `[`(e: ArithExpr) = new {
      def `/`(a: NamedVar) = new {
        def `]` = PhraseType.substitute(e, `for`=a, in=t)
      }
    }
  }

  implicit class PairTypeConstructor[T1 <: PhraseType](t1: T1) {
    def x[T2 <: PhraseType](t2: T2) = PairType(t1, t2)
  }

  implicit class FunctionTypeConstructor[T1 <: PhraseType](t1: T1) {
    def ->[T2 <: PhraseType](t2: T2) = FunctionType(t1, t2)
  }

  implicit class PassiveFunctionTypeConstructor[T1 <: PhraseType](t1: T1) {
    def `->p`[T2 <: PhraseType](t2: T2) = PassiveFunctionType(t1, t2)
  }

  implicit class NatDependentFunctionTypeConstructor(x: NamedVar) {
    def ->[T <: PhraseType](outT: T) = NatDependentFunctionType(x, outT)
  }

  implicit class PhraseTypeHelper(val sc: StringContext) extends AnyVal {
    def t(args: Any*): PhraseType = {
      new PhraseTypeParser(sc.s(args:_*), sc.parts.iterator, args.iterator).parsePhraseType
    }

    def exp(args: Any*): ExpType = {
      new PhraseTypeParser(sc.s(args:_*), sc.parts.iterator.drop(1), args.iterator).parseExpType
    }

    def acc(args: Any*): AccType = {
      new PhraseTypeParser(sc.s(args:_*), sc.parts.iterator.drop(1), args.iterator).parseAccType
    }
  }

}
