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
    ParFor(n, dt, out, fun(expT(idx(n), read))(i => fun(accT(dt))(o => f(i)(o) )))
}

object `parForVec` {
  def apply(n: Nat,
            st: DataType,
            out: Phrase[AccType],
            f: Phrase[ExpType] => Phrase[AccType] => Phrase[CommType]): ForVec =
    ForVec(n, st, out, fun(expT(idx(n), read))(i => fun(accT(st))(o => f(i)(o) )))
}

object parForNat {
  def apply(n: Nat, ft: NatToData, out: Phrase[AccType],
            f: NatIdentifier => Phrase[AccType] => Phrase[CommType]): ParForNat = {
    ParForNat(n, ft, out, nFun(idx => fun(accT(ft(idx)))(o => f(idx)(o)), RangeAdd(0, n, 1)))
  }
}
