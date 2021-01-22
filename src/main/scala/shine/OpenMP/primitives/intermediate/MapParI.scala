package shine.OpenMP.primitives.intermediate

import shine.DPIA.DSL._
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenMP.DSL.parFor

object MapParI {
  def apply(n: Nat, dt1: DataType, dt2: DataType,
            f: Phrase[ExpType ->: AccType ->: CommType],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommType] = {
    parFor(n, dt2, out, i => a => f(in `@` i)(a))
  }
}
