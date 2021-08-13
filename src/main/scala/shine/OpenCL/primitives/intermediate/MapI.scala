package shine.OpenCL.primitives.intermediate

import rise.core.DSL.Type._
import rise.core.types.{DataType, read}
import shine.DPIA.DSL._
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL._

final case class MapI(level: ParallelismLevel, dim: Int) {
  def apply(n: Nat, dt1: DataType, dt2: DataType,
            f: Phrase[ExpType ->: AccType ->: CommType],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommType] = {
    comment(s"map${level.toString}") `;`
      shine.OpenCL.DSL.parFor(level, dim, unroll = false)(n, dt2, out,
        fun(expT(idx(n), read))(i => fun(accT(dt2))(a => f(in `@` i)(a))))
  }
}
