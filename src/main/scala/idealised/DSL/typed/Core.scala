package idealised.DSL.typed

import idealised.Core._
import lift.arithmetic.NamedVar

object identifier {
  def apply[T <: PhraseType](name: String, t: T) = Identifier(name, t)
}

trait funDef {

  def apply[T1 <: PhraseType, T2 <: PhraseType](t: T1)
                                               (f: Identifier[T1] => Phrase[T2]): Lambda[T1, T2] = {
    val param = identifier(newName(), t)
    Lambda(param, f(param))
  }

}

object fun extends funDef

object \ extends funDef

object λ extends funDef

trait dependentFunDef {

  def apply[T <: PhraseType](f: NamedVar => Phrase[T]) = {
    val x = NamedVar(newName())
    NatDependentLambda(x, f(x))
  }

  def apply[T <: PhraseType](f: NamedVar => Phrase[T], range: lift.arithmetic.Range) = {
    val x = NamedVar(newName(), range)
    NatDependentLambda(x, f(x))
  }

  def apply[T <: PhraseType](f: DataTypeIdentifier => Phrase[T]) = {
    val x = DataTypeIdentifier(newName())
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
