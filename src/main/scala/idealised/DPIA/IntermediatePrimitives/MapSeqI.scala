package idealised.DPIA.IntermediatePrimitives

import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._

object MapSeqI {
  def apply(n: Nat, dt1: DataType, dt2: DataType,
            f: Phrase[ExpType -> (AccType -> CommandType)],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommandType] =
  {
    `for`(n, i => f(in `@` i)(out `@` i))
  }
}
