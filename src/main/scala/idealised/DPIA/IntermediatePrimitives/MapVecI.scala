package idealised.DPIA.IntermediatePrimitives

import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenMP.DSL.parForVec

object MapVecI {
  def apply(n: Nat, st1: ScalarType, st2: ScalarType,
            f: Phrase[ExpType ->: AccType ->: CommType],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommType] =
  {
    parForVec(n, st2, out, i => a => f(in `@v` i)(a))
  }
}
