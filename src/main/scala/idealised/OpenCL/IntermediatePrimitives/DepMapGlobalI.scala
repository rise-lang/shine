package idealised.OpenCL.IntermediatePrimitives

import idealised.DPIA.DSL._
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommType, ExpType}
import idealised.DPIA._
import idealised.OpenCL.DSL.parForNatGlobal

final case class DepMapGlobalI(dim:Int) {
  def apply(n: Nat,
            ft1:NatDataTypeFunction,
            ft2:NatDataTypeFunction,
            f: Phrase[`(nat)->`[ExpType -> (AccType -> CommType)]],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommType] =
  {
    parForNatGlobal(dim)(n, ft2, out, idx => a => f(idx)(in `@d` idx)(a))
  }
}


