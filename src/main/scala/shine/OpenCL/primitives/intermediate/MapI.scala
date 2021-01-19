package shine.OpenCL.primitives.intermediate

import shine.DPIA.DSL._
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.DataType.idx
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL._
import shine.OpenCL.primitives.imperative.ParFor

final case class MapI(level: ParallelismLevel, dim: Int) {
  def apply(n: Nat, dt1: DataType, dt2: DataType,
            f: Phrase[ExpType ->: AccType ->: CommType],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommType] = {
    comment(s"map${level.toString}") `;`
      ParFor(level, dim, unroll = false)(n, dt2, out,
        λ(expT(idx(n), read))(i => λ(accT(dt2))(a => f(in `@` i)(a))))
  }
}
