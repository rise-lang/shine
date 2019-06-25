package idealised.OpenCL.IntermediatePrimitives
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.DSL.parForNatWorkGroup

final case class DepMapWorkGroupI(dim:Int) {
  def apply(n: Nat,
            ft1:NatToData,
            ft2:NatToData,
            f: Phrase[`(nat)->`[ExpType -> (AccType -> CommType)]],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommType] =
  {
    parForNatWorkGroup(dim)(n, ft2, out, idx => a => f(idx)(in `@d` idx)(a))
  }
}
