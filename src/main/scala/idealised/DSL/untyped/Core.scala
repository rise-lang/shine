package idealised.DSL.untyped

import idealised.Core._
import idealised.LowLevelCombinators.Record
import apart.arithmetic.NamedVar

object identifier {
  def apply[T <: PhraseType](name: String, t: T) = IdentPhrase(name, t)
}

trait funDef {
  def apply[T <: PhraseType](f: IdentPhrase[ExpType] => Phrase[T]): LambdaPhrase[ExpType, T] = {
    apply[ExpType, T](null)(f)
  }

  def apply[T <: PhraseType](f: (Phrase[ExpType], Phrase[ExpType]) => Phrase[T]): LambdaPhrase[ExpType x ExpType, T] = {
    apply[ExpType x ExpType, T](null)(x => f(π1(x), π2(x)))
  }

  def apply[T1 <: PhraseType, T2 <: PhraseType](t: T1)
                                               (f: IdentPhrase[T1] => Phrase[T2]): LambdaPhrase[T1, T2] = {
    val param = identifier(newName(), t)
    LambdaPhrase(param, f(param))
  }

}

object fun extends funDef

object \ extends funDef

object λ extends funDef


trait dependentFunDef {

  def apply[T <: PhraseType](f: NamedVar => Phrase[T]): NatDependentLambdaPhrase[T] = {
    val x = NamedVar(newName())
    NatDependentLambdaPhrase(x, f(x))
  }

  def apply[T <: PhraseType](f: DataTypeIdentifier => Phrase[T]): TypeDependentLambdaPhrase[T] = {
    val x = DataTypeIdentifier(newName())
    TypeDependentLambdaPhrase(x, f(x))
  }

}

object _Λ_ extends dependentFunDef

object π1 {
  def apply[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2]) =
    Proj1Phrase(pair)
}

object π2 {
  def apply[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2]) =
    Proj2Phrase(pair)
}

object record {
  def apply(fst: Phrase[ExpType], snd: Phrase[ExpType]) = Record(null, null, fst, snd)
}
