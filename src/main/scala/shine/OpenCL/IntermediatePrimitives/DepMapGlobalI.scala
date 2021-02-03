package shine.OpenCL.IntermediatePrimitives

import shine.DPIA.DSL.{λ, _}
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.DSL.parForNatGlobal
import shine.OpenCL.ImperativePrimitives.ParForNatGlobal

final case class DepMapGlobalI(dim:Int) {
  def apply(n: Nat,
            ft1:NatToData,
            ft2:NatToData,
            f: Phrase[`(nat)->:`[ExpType ->: AccType ->: CommType]],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommType] =
  {
    ParForNatGlobal(dim)(n, ft2, out, nFun(idx => λ(accT(ft2(idx)))(a => f(idx)(in `@d` idx)(a)), arithexpr.arithmetic.RangeUnknown))
  }
}


