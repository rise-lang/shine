package idealised

import idealised.SurfaceLanguage.Types._
import lift.arithmetic.{ArithExpr, NamedVar}

package object SurfaceLanguage {

  // reverse function application in the style of F#
  implicit class Pipe[A](val a: A) extends AnyVal {
    def |>[B](f: A => B): B = f(a)
  }

  object newName {
    var counter = 0

    def apply(): String = {
      counter += 1
      "v" + counter
    }
  }

  type Nat = ArithExpr
  type NatIdentifier = NamedVar

  type ->[T1 <: Type, T2 <: Type] = FunctionType[T1, T2]
  type `(nat)->`[T <: Type] = NatDependentFunctionType[T]
  type `(dt)->`[T <: Type] = TypeDependentFunctionType[T]

  type NatDependentDataType = NatDependentFunctionType[DataType]
  type `(nat)->dt` = NatDependentDataType
}
