package idealised.OpenCL.IntermediatePrimitives
import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType}
import idealised.DPIA._
import idealised.OpenCL.DSL.parForNatLocal

final case class DepMapLocalI(dim:Int) {
  def apply(n: Nat,
            ft1:NatDataTypeFunction,
            ft2:NatDataTypeFunction,
            f: Phrase[`(nat)->`[ExpType -> (AccType -> CommandType)]],
            in: Phrase[ExpType],
            out: Phrase[AccType])
           (implicit context: TranslationContext): Phrase[CommandType] =
  {
    parForNatLocal(dim)(n, ft2.x, ft2.body, out, idx => a => f(idx)(in `@d` idx)(a))
  }
}