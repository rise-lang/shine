package shine.DPIA.DSL

import rise.core.types.{Kind, NatIdentifier, NatKind}
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._

object identifier {
  def apply[T <: PhraseType](name: String, t: T): Identifier[T] = Identifier(name, t)
}

case class fun[T <: PhraseType](t: T) {
  def apply[U <: PhraseType](f: Identifier[T] => Phrase[U]): Lambda[T, U] = {
    val param = identifier(freshName("x"), t)
    Lambda(param, f(param))
  }
}

case class depFun[T, I](kind: Kind[T, I]) {
  def apply[U <: PhraseType](f: I => Phrase[U]): DepLambda[T, I, U] = {
    val x = Kind.makeIdentifier(kind)
    DepLambda(kind, x, f(x))
  }
}

object nFun {
  def apply[T <: PhraseType](f: NatIdentifier => Phrase[T],
                             range: arithexpr.arithmetic.Range): DepLambda[Nat, NatIdentifier, T] = {
    val x = NatIdentifier(freshName("n"), range)
    DepLambda(NatKind, x, f(x))
  }
}
