package shine.DPIA.IntermediatePrimitives

import arithexpr.arithmetic.RangeAdd
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.ForNat
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._

object DepMapSeqI {
  def apply(n: Nat,
            ft1:NatToData,
            ft2:NatToData,
            f: Phrase[`(nat)->:`[ExpType ->: AccType ->: CommType]],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommType] =
  {
    ForNat(n, nFun(i => f(i)(in `@d` i)(out `@d` i), RangeAdd(0, n, 1)), unroll = false)
  }
}

object DepMapSeqIUnroll {
  def apply(n: Nat,
            ft1:NatToData,
            ft2:NatToData,
            f: Phrase[`(nat)->:`[ExpType ->: AccType ->: CommType]],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommType] =
  {
    ForNat(n, nFun(i => f(i)(in `@d` i)(out `@d` i), RangeAdd(0, n, 1)), unroll = true)
  }
}