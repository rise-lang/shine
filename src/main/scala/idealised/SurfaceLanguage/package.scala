package idealised

import idealised.SurfaceLanguage.Types._
import lift.arithmetic.{ArithExpr, NamedVar}

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
  type NatFunIdentifier = NamedVar

  type ->[T1 <: Type, T2 <: Type] = FunctionType[T1, T2]
  type `(nat)->`[T <: Type] = NatDependentFunctionType[T]
  type `(dt)->`[T <: Type] = TypeDependentFunctionType[T]

  type NatDependentDataType = NatDependentFunctionType[DataType]
  type `(nat)->dt` = NatDependentDataType
}
