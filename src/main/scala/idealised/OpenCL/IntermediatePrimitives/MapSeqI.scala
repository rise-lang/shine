package idealised.OpenCL.IntermediatePrimitives

import idealised.DPIA.DSL.{`for`, _}
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType}
import idealised.DPIA._

//noinspection TypeAnnotation
object MapSeqI {
  def apply(n: Nat, dt1: DataType, dt2: DataType,
            f: Phrase[ExpType -> (AccType -> CommandType)],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommandType] =
  {
    `for`(n, i => f(in `@` i)(out `@` i))
  }
}
