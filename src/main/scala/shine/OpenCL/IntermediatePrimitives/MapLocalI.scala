package shine.OpenCL.IntermediatePrimitives

import shine.DPIA.DSL._
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType, read}
import shine.DPIA.Types.DataType._
import shine.DPIA._
import shine.OpenCL.DSL.barrier
import shine.OpenCL.ImperativePrimitives.ParForLocal

final case class MapLocalI(dim: Int) {
  def apply(n: Nat, dt1: DataType, dt2: DataType,
            f: Phrase[ExpType ->: AccType ->: CommType],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommType] =
  {
    comment("mapLocal")`;`
    ParForLocal(dim)(n, dt2, out,
      λ(expT(idx(n), read))(i => λ(accT(dt2))(a => f(in `@` i)(a)))) `;`
    barrier()
  }
}
