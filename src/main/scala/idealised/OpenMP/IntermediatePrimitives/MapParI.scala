package idealised.OpenMP.IntermediatePrimitives

import idealised.DPIA.DSL._
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType}
import idealised.DPIA._
import idealised.OpenMP.DSL._

object MapParI {
  def apply(n: Nat, dt1: DataType, dt2: DataType,
            f: Phrase[ExpType -> (AccType -> CommandType)],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommandType] =
  {
    parFor(n, dt2, out, i => a => f(in `@` i)(a))
  }
}
