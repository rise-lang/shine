package idealised.DPIA.DSL

import idealised.DPIA.Phrases._
import idealised.DPIA.Types.{DataTypeIdentifier, PhraseType}
import idealised.DPIA._
import lift.arithmetic.PosVar

object identifier {
  def apply[T <: PhraseType](name: String, t: T) = Identifier(name, t)
}

trait funDef {

  def apply[T1 <: PhraseType, T2 <: PhraseType](t: T1)
                                               (f: Identifier[T1] => Phrase[T2]): Lambda[T1, T2] = {
    val param = identifier(freshName(), t)
    Lambda(param, f(param))
  }

}

object fun extends funDef

object \ extends funDef

object λ extends funDef

trait dependentFunDef {

  def apply[T <: PhraseType](f: PosVar => Phrase[T]): NatDependentLambda[T] = {
    val x = PosVar(freshName())
    NatDependentLambda(x, f(x))
  }

  def apply[T <: PhraseType](f: PosVar => Phrase[T],
                             range: lift.arithmetic.Range): NatDependentLambda[T] = {
    val x = PosVar(freshName(), range)
    NatDependentLambda(x, f(x))
  }

  def apply[T <: PhraseType](f: DataTypeIdentifier => Phrase[T]): TypeDependentLambda[T] = {
    val x = DataTypeIdentifier(freshName())
    TypeDependentLambda(x, f(x))
  }

}

object _Λ_ extends dependentFunDef

object π1 {
  def apply[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2]) =
    Proj1(pair)
}

object π2 {
  def apply[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2]) =
    Proj2(pair)
}
