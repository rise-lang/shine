package idealised.DPIA.IntermediatePrimitives

import idealised.DPIA.DSL._
import idealised.DPIA.ImperativePrimitives.ForNat
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._
import lift.arithmetic.RangeAdd

object DepMapSeqI {
  def apply(n: Nat,
            i1: NatIdentifier, dt1: DataType,
            i2: NatIdentifier, dt2: DataType,
            f: Phrase[`(nat)->`[ExpType -> (AccType -> CommandType)]],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommandType] =
  {
    ForNat(n, _Λ_( i => f(i)(in `@d` i)(out `@d` i), RangeAdd(0, n, 1)))
  }
}