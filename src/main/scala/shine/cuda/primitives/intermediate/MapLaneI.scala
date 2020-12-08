package shine.cuda.primitives.intermediate

import shine.DPIA.DSL.{λ, _}
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.DataType._
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType, read}
import shine.DPIA._
import shine.cuda.primitives.imperative.{ParForLane, SyncWarp}

final case class MapLaneI(dim: Char = 'x') {
  def apply(n: Nat, dt1: DataType, dt2: DataType,
            f: Phrase[ExpType ->: AccType ->: CommType],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommType] =
  {
    comment("mapLane")`;`
      ParForLane(dim)(n, dt2, out,
        λ(expT(idx(n), read))(i => λ(accT(dt2))(a => f(in `@` i)(a))))
//      SyncWarp()
  }
}
