package idealised.DSL.untyped

import idealised.Core._
import idealised.FunctionalPrimitives._
import lift.arithmetic.NamedVar


object map {
  def apply(f: Phrase[ExpType -> ExpType]): Phrase[ExpType -> ExpType] =
    λ(x => map(f, x))

  def apply(f: Phrase[ExpType -> ExpType], x: Phrase[ExpType]): Map =
    Map(null, null, null, f, x)
}

object zip {
  def apply(lhs: Phrase[ExpType], rhs: Phrase[ExpType]): Zip =
    Zip(null, null, null, lhs, rhs)
}

object split {
  def apply(n: Nat): Phrase[ExpType -> ExpType] =
    λ(array => split(n, array))

  def apply(n: Nat, array: Phrase[ExpType]): Split =
    Split(n, null, null, array)
}

object join {
  def apply(): Phrase[ExpType -> ExpType] = λ(array => join(array))

  def apply(array: Phrase[ExpType]): Join = Join(null, null, null, array)
}

object reduce {
  def apply(f: Phrase[ExpType -> (ExpType -> ExpType)]): Phrase[(ExpType x ExpType) -> ExpType] =
    λ((init, array) => reduce(f, init, array))

  def apply(f: Phrase[ExpType -> (ExpType -> ExpType)],
            init: Phrase[ExpType]): Phrase[ExpType -> ExpType] =
    λ(array => reduce(f, init, array))

  def apply(f: Phrase[ExpType -> (ExpType -> ExpType)], init: Phrase[ExpType],
            array: Phrase[ExpType]): Reduce =
    Reduce(null, null, null, f, init, array)
}

object iterate {
  def apply(k: Nat, f: Phrase[`(nat)->`[ExpType -> ExpType]]): Phrase[ExpType -> ExpType] =
    λ(array => iterate(k, f, array))

  def apply(k: Nat,
            f: Phrase[`(nat)->`[ExpType -> ExpType]],
            array: Phrase[ExpType]): Iterate =
    Iterate(null, null, k, null, f, array)
}

object gather {
  def apply(idxF: Phrase[`(nat)->`[ExpType ->ExpType]]): Phrase[ExpType -> ExpType] =
    λ(array => Gather(null, null, idxF(NamedVar(newName())), array))
}
