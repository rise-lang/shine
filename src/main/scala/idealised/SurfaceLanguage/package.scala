package idealised

import idealised.SurfaceLanguage.Types._
import lift.arithmetic.{ArithExpr, NamedVar, Var}

package object SurfaceLanguage {

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

  implicit class ExprSubstitutionHelper[T1 <: Type](in: Expr[T1]) {
    def `[`[T2 <: Type](e: Expr[T2]) = new {
      def `/`(`for`: Expr[T2]) = new {
        def `]`: Expr[T1] = Expr.substitute(e, `for`, in)
      }
    }

    def `[`(e: Nat) = new {
      def `/`(`for`: NatIdentifier) = new {
        def `]`: Expr[T1] = Type.substitute(e, `for`, in)
      }
    }

    def `[`(dt: DataType) = new {
      def `/`(`for`: DataTypeIdentifier) = new {
        def `]`: Expr[T1] = Type.substitute(dt, `for`, in)
      }
    }
  }

  implicit class FunctionTypeConstructor(dt: DataType) {
    def ->[T <: Type](t: T) = FunctionType(dt, t)
  }

  implicit class NatSubstituionHelpfer(in: Nat) {
    def `[`(ae: Nat) = new {
      def `/`(`for`: NatIdentifier) = new {
        def `]`: Nat = {
          in.visitAndRebuild {
            case v: Var =>
              if (`for`.name == v.name) {
                ae
              } else {
                v
              }
            case e => e
          }
        }
      }
    }
  }
}
