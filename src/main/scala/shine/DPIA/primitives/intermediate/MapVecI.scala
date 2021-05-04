package shine.DPIA.primitives.intermediate

import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenMP.DSL.parForVec

object MapVecI {
  def apply(n: Nat, st1: DataType, st2: DataType,
            f: Phrase[ExpType ->: AccType ->: CommType],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommType] =
  {
    parForVec(n, st2, out, i => a => f(in `@v` i)(a))
  }
}
