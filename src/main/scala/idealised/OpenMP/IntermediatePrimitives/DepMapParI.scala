package idealised.OpenMP.IntermediatePrimitives

import idealised.DPIA.DSL._
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType}
import idealised.DPIA._
import idealised.OpenMP.DSL.parForNat

//noinspection TypeAnnotation
object DepMapParI {
  def apply(n: Nat,
            i1: NatIdentifier, dt1: DataType,
            i2: NatIdentifier, dt2: DataType,
            f: Phrase[`(nat)->`[ExpType -> (AccType -> CommandType)]],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommandType] =
  {
    parForNat(n, i2, dt2, out, idx => a => f(idx)(in `@d` idx)(a))
  }
}