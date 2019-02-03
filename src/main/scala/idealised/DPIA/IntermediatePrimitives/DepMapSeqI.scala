package idealised.DPIA.IntermediatePrimitives

import idealised.DPIA.Compilation.{TranslationContext, SubstituteImplementations}
import idealised.DPIA.DSL._
import idealised.DPIA.ImperativePrimitives.ForNat
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._
import lift.arithmetic.RangeAdd

final case class DepMapSeqI(n: Nat,
                            i1: NatIdentifier, dt1: DataType,
                            i2: NatIdentifier, dt2: DataType,
                            f: Phrase[`(nat)->`[ExpType -> (AccType -> CommandType)]],
                            in: Phrase[ExpType],
                            out: Phrase[AccType])
  extends AbstractDepMapI(n, i1, dt1, i2, dt2, f, in, out) {

  override def makeMapI: (Nat, NatIdentifier, DataType, NatIdentifier, DataType, Phrase[`(nat)->`[ExpType -> (AccType -> CommandType)]], Phrase[ExpType], Phrase[AccType]) => DepMapSeqI = DepMapSeqI

  override def substituteImpl(env: SubstituteImplementations.Environment)
                             (implicit context: TranslationContext): Phrase[CommandType] = {
    ForNat(n, _Λ_( i =>
      SubstituteImplementations(f(i)(in `@d` i)(out `@d` i), env)
      , RangeAdd(0, n, 1))
    )
  }
}
