package idealised.OpenCL.IntermediatePrimitives
import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommType, ExpType}
import idealised.DPIA._
import idealised.OpenCL.DSL.parForNatLocal

final case class DepMapLocalI(dim:Int) {
  def apply(n: Nat,
            ft1:NatDataTypeFunction,
            ft2:NatDataTypeFunction,
            f: Phrase[`(nat)->`[ExpType -> (AccType -> CommType)]],
            in: Phrase[ExpType],
            out: Phrase[AccType])
           (implicit context: TranslationContext): Phrase[CommType] =
  {
    parForNatLocal(dim)(n, ft2, out, idx => a => f(idx)(in `@d` idx)(a))
  }
}