package shine.OpenMP.DSL

import arithexpr.arithmetic.RangeAdd
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.ForVec
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._
import shine.OpenMP.ImperativePrimitives._

object parFor {
  def apply(n: Nat,
            dt: DataType,
            out: Phrase[AccType],
            f: Phrase[ExpType] => Phrase[AccType] => Phrase[CommType]): ParFor =
    ParFor(n, dt, out, λ(expT(idx(n), read))( i => λ(accT(dt))( o => f(i)(o) )))
}

object `parForVec` {
  def apply(n: Nat,
            st: ScalarType,
            out: Phrase[AccType],
            f: Phrase[ExpType] => Phrase[AccType] => Phrase[CommType]): ForVec =
    ForVec(n, st, out, λ(expT(idx(n), read))(i => λ(accT(st))(o => f(i)(o) )))
}

object parForNat {
  def apply(n:Nat, ft:NatToData, out:Phrase[AccType], f:NatIdentifier => Phrase[AccType] => Phrase[CommType]):ParForNat = {
    ParForNat(n, ft, out, nFun(idx => λ(accT(ft(idx)))(o => f(idx)(o)), RangeAdd(0, n, 1)))
  }
}
