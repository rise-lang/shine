package idealised.DPIA.DSL

import idealised.DPIA.Phrases._
import idealised.DPIA.Types.{Kind, NatKind, PhraseType}
import idealised.DPIA._

object identifier {
  def apply[T <: PhraseType](name: String, t: T) = Identifier(name, t)
}

trait funDef {

  def apply[T1 <: PhraseType, T2 <: PhraseType](t: T1)
                                               (f: Identifier[T1] => Phrase[T2]): Lambda[T1, T2] = {
    val param = identifier(freshName("x"), t)
    Lambda(param, f(param))
  }

}

object fun extends funDef

object \ extends funDef

object λ extends funDef

trait depFunDef {

  def apply[T <: PhraseType](f: NatIdentifier => Phrase[T],
                             range: lift.arithmetic.Range): DepLambda[NatKind, T] = {
    val x = NatIdentifier(freshName("n"), range)
    DepLambda[NatKind, T](x, f(x))
  }

  def apply[K <: Kind]: Object {
    def apply[T <: PhraseType](f: K#I => Phrase[T])
                              (implicit w: Kind.IdentifierMaker[K]): DepLambda[K, T]
  } = new {
    def apply[T <: PhraseType](f: K#I => Phrase[T])
                              (implicit w: Kind.IdentifierMaker[K]): DepLambda[K, T] = {
      val x = w.makeIdentifier()
      DepLambda[K, T](x, f(x))
    }
  }

}

object depFun extends funDef

object _Λ_ extends depFunDef

object π1 {
  def apply[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2]) =
    Proj1(pair)
}

object π2 {
  def apply[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2]) =
    Proj2(pair)
}
