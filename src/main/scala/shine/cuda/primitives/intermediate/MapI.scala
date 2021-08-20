package shine.cuda.primitives.intermediate

import rise.core.DSL.Type._
import rise.core.types.{DataType, read}
import shine.DPIA.DSL._
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL._
import shine.cuda.primitives.imperative.{ParFor, SyncThreads, SyncWarp}

final case class MapI(level: ParallelismLevel, dim: Int) {
  def apply(n: Nat, dt1: DataType, dt2: DataType,
            f: Phrase[ExpType ->: AccType ->: CommType],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommType] = {
    val imperativ = comment(s"map${level.toString}") `;`
                      shine.cuda.DSL.parFor(level, dim, unroll = false)(n, dt2, out,
                        fun(expT(idx(n), read))(i => fun(accT(dt2))(a => f(in `@` i)(a))))
    //TODO use other InsertMemoryBarrieres-mechanism
    level match {
      case Local => imperativ `;` SyncThreads()
      case Warp => imperativ `;` SyncThreads()
      case Lane => imperativ `;` SyncWarp()
      case _ => imperativ
    }
  }
}

