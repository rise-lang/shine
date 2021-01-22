package shine.OpenCL.primitives.intermediate

import shine.DPIA.DSL._
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.DSL.parForNatGlobal

final case class DepMapGlobalI(dim:Int) {
  def apply(n: Nat,
            ft1:NatToData,
            ft2:NatToData,
            f: Phrase[`(nat)->:`[ExpType ->: AccType ->: CommType]],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommType] =
  {
    parForNatGlobal(dim)(n, ft2, out, idx => a => f(idx)(in `@d` idx)(a))
  }
}


