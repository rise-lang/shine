package shine.DPIA.primitives.intermediate

import arithexpr.arithmetic.RangeAdd
import rise.core.types.NatToData
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.imperative.ForNat

object DepMapSeqI {
  def apply(unroll: Boolean)
           (n: Nat,
            ft1:NatToData,
            ft2:NatToData,
            f: Phrase[`(nat)->:`[ExpType ->: AccType ->: CommType]],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommType] =
  {
    ForNat(unroll)(n, nFun(i => f(i)(in `@d` i)(out `@d` i), RangeAdd(0, n, 1)))
  }
}
