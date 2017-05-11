package idealised.DSL.untyped

import idealised.Core._
import idealised.FunctionalPrimitives.Record
import lift.arithmetic.NamedVar

object identifier {
  def apply[T <: PhraseType](name: String, t: T): Identifier[T] = Identifier(name, t)
}

trait funDef {
  def apply[T <: PhraseType](f: Identifier[ExpType] => Phrase[T]): Lambda[ExpType, T] =
    apply[ExpType, T](null)(f)

  def apply[T <: PhraseType](f: (Phrase[ExpType], Phrase[ExpType]) => Phrase[T]): Lambda[ExpType x ExpType, T] =
    apply[ExpType x ExpType, T](null)(x => f(π1(x), π2(x)))

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

  def apply[T <: PhraseType](f: NamedVar => Phrase[T]): NatDependentLambda[T] = {
    val x = NamedVar(newName())
    NatDependentLambda(x, f(x))
  }

  def apply[T <: PhraseType](f: DataTypeIdentifier => Phrase[T]): TypeDependentLambda[T] = {
    val x = DataTypeIdentifier(newName())
    TypeDependentLambda(x, f(x))
  }

}

object _Λ_ extends dependentFunDef

object π1 {
  def apply[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2]): Proj1[T1, T2] = Proj1(pair)
}

object π2 {
  def apply[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2]): Proj2[T1, T2] = Proj2(pair)
}

object record {
  def apply(fst: Phrase[ExpType], snd: Phrase[ExpType]): Record = Record(null, null, fst, snd)
}
