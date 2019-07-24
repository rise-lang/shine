package idealised.OpenCL.IntermediatePrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.DSL.{λ, _}
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommType, DataType, ExpType}
import idealised.DPIA._
import idealised.OpenCL.ImperativePrimitives.ParForLocal

final case class MapLocalI(dim: Int) {
  def apply(n: Nat, dt1: DataType, dt2: DataType,
            f: Phrase[ExpType ->: AccType ->: CommType],
            in: Phrase[ExpType],
            out: Phrase[AccType])
           (implicit context: TranslationContext): Phrase[CommType] =
  {
    ParForLocal(dim)(n, dt2, out,
      λ(exp"[idx($n)]")(i => λ(acc"[$dt2]")(a => f(in `@` i)(a))))
  }
}
