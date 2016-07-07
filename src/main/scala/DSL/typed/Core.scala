package DSL.typed

import Core._
import apart.arithmetic.NamedVar

object identifier {
  def apply[T <: PhraseType](name: String, t: T) = IdentPhrase(name, t)
}

trait funDef {

  def apply[T1 <: PhraseType, T2 <: PhraseType](t: T1)
                                               (f: IdentPhrase[T1] => Phrase[T2]): LambdaPhrase[T1, T2] = {
    val param = identifier(newName(), t)
    LambdaPhrase(param, f(param))
  }

}

object fun extends funDef

object \ extends funDef

object λ extends funDef

trait natDependentFunDef {

  def apply[T <: PhraseType](f: NamedVar => Phrase[T]): NatDependentLambdaPhrase[T] = {
    val x = NamedVar(newName())
    NatDependentLambdaPhrase(x, f(x))
  }

}

object _Λ_ extends natDependentFunDef

object π1 {
  def apply[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2]) =
    Proj1Phrase(pair)
}

object π2 {
  def apply[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2]) =
    Proj2Phrase(pair)
}
