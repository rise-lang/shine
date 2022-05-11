package shine.DPIA.DSL

import rise.core.types.{Kind, NatIdentifier, NatKind}
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._

object identifier {
  def apply[T <: PhraseType](name: String, t: T): Identifier[T] = Identifier(name, t)
}

case class fun[T1 <: PhraseType, T2 <: PhraseType](t: T1) {
  def apply(f: Identifier[T1] => Phrase[T2]): Lambda[T1, T2] = {
    val param = identifier(freshName("x"), t)
    Lambda(param, f(param))
  }
}

case class nFun(range: arithexpr.arithmetic.Range) {
  def apply[T <: PhraseType](f: NatIdentifier => Phrase[T]): DepLambda[Nat, NatIdentifier, T] = {
    val x = NatIdentifier(freshName("n"), range)
    DepLambda(NatKind, x, f(x))
  }
}

case class depFun[T, I](kind: Kind[T, I]) {
  def apply[U <: PhraseType](f: I => Phrase[U]): DepLambda[T, I, U] = {
    val x = Kind.makeIdentifier(kind)
    DepLambda(kind, x, f(x))
  }
}
