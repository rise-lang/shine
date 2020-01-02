package shine.OpenCL.IntermediatePrimitives

import shine.DPIA.DSL._
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType, read}
import shine.DPIA._
import shine.OpenCL.ImperativePrimitives.ParForGlobal

final case class MapGlobalI(dim: Int) {
  def apply(n: Nat, dt1: DataType, dt2: DataType,
            f: Phrase[ExpType ->: AccType ->: CommType],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommType] =
  {
    comment("mapGlobal")`;`
    ParForGlobal(dim)(n, dt2, out,
      λ(exp"[idx($n), $read]")(i => λ(acc"[$dt2]")(a => f(in `@` i)(a))))
  }
}
