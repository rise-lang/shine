package idealised.OpenCL.IntermediatePrimitives
import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType}
import idealised.DPIA._
import idealised.OpenCL.DSL.parForNatLocal

final case class DepMapLocalI(dim:Int) {
  def apply(n: Nat,
            i1: NatIdentifier, dt1: DataType,
            i2: NatIdentifier, dt2: DataType,
            f: Phrase[`(nat)->`[ExpType -> (AccType -> CommandType)]],
            in: Phrase[ExpType],
            out: Phrase[AccType])
           (implicit context: TranslationContext): Phrase[CommandType] =
  {
    parForNatLocal(dim)(n, i2, dt2, out, idx => a => f(idx)(in `@d` idx)(a))
  }
}