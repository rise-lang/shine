package idealised.OpenCL.IntermediatePrimitives
import idealised.DPIA.Compilation.{SubstituteImplementations, TranslationContext}
import idealised.DPIA.DSL._
import idealised.DPIA.IntermediatePrimitives.AbstractDepMapI
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType}
import idealised.DPIA._
import idealised.OpenCL.DSL.parForNatWorkGroup

final case class DepMapWorkGroupI(dim:Int)(n: Nat,
                                        i1: NatIdentifier, dt1: DataType,
                                        i2: NatIdentifier, dt2: DataType,
                                        f: Phrase[`(nat)->`[ExpType -> (AccType -> CommandType)]],
                                        in: Phrase[ExpType],
                                        out: Phrase[AccType])
  extends AbstractDepMapI(n, i1, dt1, i2, dt2, f, in, out) {

  override def makeMapI:
  (Nat,
    NatIdentifier, DataType,
    NatIdentifier, DataType,
    Phrase[`(nat)->`[ExpType -> (AccType -> CommandType)]],
    Phrase[ExpType], Phrase[AccType]) => DepMapWorkGroupI = DepMapWorkGroupI(dim)

  override def substituteImpl(env: SubstituteImplementations.Environment)(implicit context: TranslationContext): Phrase[CommandType] = {
    parForNatWorkGroup(dim)(n, i2, dt2, out, idx => a => SubstituteImplementations(f(idx)(in `@d` idx)(a), env))
  }
}
