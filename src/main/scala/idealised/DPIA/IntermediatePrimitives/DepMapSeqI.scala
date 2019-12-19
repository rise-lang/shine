package idealised.DPIA.IntermediatePrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.DSL._
import idealised.DPIA.ImperativePrimitives.ForNat
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._
import rise.arithmetic.RangeAdd

object DepMapSeqI {
  def apply(n: Nat,
            ft1:NatToData,
            ft2:NatToData,
            f: Phrase[`(nat)->:`[ExpType ->: AccType ->: CommType]],
            in: Phrase[ExpType],
            out: Phrase[AccType])
           (implicit context: TranslationContext): Phrase[CommType] =
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
            out: Phrase[AccType])
           (implicit context: TranslationContext): Phrase[CommType] =
  {
    ForNat(n, nFun(i => f(i)(in `@d` i)(out `@d` i), RangeAdd(0, n, 1)), unroll = true)
  }
}