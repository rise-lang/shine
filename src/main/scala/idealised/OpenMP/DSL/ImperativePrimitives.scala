package idealised.OpenMP.DSL

import idealised.DPIA.DSL._
import idealised.DPIA.ImperativePrimitives.ForVec
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenMP.ImperativePrimitives._
import lift.arithmetic.RangeAdd

object parFor {
  def apply(n: Nat,
            dt: DataType,
            out: Phrase[AccType],
            f: Phrase[ExpType] => Phrase[AccType] => Phrase[CommandType]): ParFor =
    ParFor(n, dt, out, λ(exp"[idx($n)]")( i => λ(acc"[$dt]")( o => f(i)(o) )))
}

object `parForVec` {
  def apply(n: Nat,
            st: ScalarType,
            out: Phrase[AccType],
            f: Phrase[ExpType] => Phrase[AccType] => Phrase[CommandType]): ForVec =
    ForVec(n, st, out, λ(exp"[idx($n)]")(i => λ(acc"[$st]")(o => f(i)(o) )))
}

object parForNat {
  def apply(n:Nat, i:NatIdentifier, dt:DataType, out:Phrase[AccType], f:NatIdentifier => Phrase[AccType] => Phrase[CommandType]):ParForNat = {
    def makeDt(x:Nat) = DataType.substitute(x, `for`=i, `in`=dt)
    ParForNat(n, i,  dt, out, _Λ_(idx => λ(acc"[${makeDt(idx)}]")(o => f(idx)(o)), RangeAdd(0, n, 1)))
  }
}
