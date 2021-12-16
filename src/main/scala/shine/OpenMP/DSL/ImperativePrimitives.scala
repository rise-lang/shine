package shine.OpenMP.DSL

import arithexpr.arithmetic.RangeAdd
import rise.core.types.{DataType, NatToData, read}
import shine.DPIA.DSL._
import shine.DPIA.primitives.imperative.ForVec
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types._
import rise.core.DSL.Type._
import shine.DPIA._
import shine.OpenMP.primitives.imperative._

object parFor {
  def apply(n: Nat,
            dt: DataType,
            out: Phrase[AccType],
            f: Phrase[ExpType] => Phrase[AccType] => Phrase[CommType]): ParFor =
    ParFor(n, dt, out, λ(expT(idx(n), read))(i => λ(accT(dt))(o => f(i)(o) )))
}

object `parForVec` {
  def apply(n: Nat,
            st: DataType,
            out: Phrase[AccType],
            f: Phrase[ExpType] => Phrase[AccType] => Phrase[CommType]): ForVec =
    ForVec(n, st, out, λ(expT(idx(n), read))(i => λ(accT(st))(o => f(i)(o) )))
}

object parForNat {
  def apply(n: Nat, ft: NatToData, out: Phrase[AccType],
            f: NatIdentifier => Phrase[AccType] => Phrase[CommType]): ParForNat = {
    ParForNat(n, ft, out, nFun(idx => λ(accT(ft(idx)))(o => f(idx)(o)), RangeAdd(0, n, 1)))
  }
}
