package shine.cuda

import shine.DPIA.Nat
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.OpenCL._
import shine.cuda.primitives.imperative.ParFor

package object DSL {
  def parFor(level: ParallelismLevel,
             dim: Int,
             unroll: Boolean
            ): (Nat, DataType, Phrase[AccType], Phrase[FunType[ExpType, FunType[AccType, CommType]]]) => ParFor =
    level match {
      case Global =>    ParFor(level, dim, unroll, "gl_id_")(
        globalId(dim), _, globalDim(dim), _, _, _)
      case Local =>     ParFor(level, dim, unroll, "tid_")(
        threadId(dim), _, blockDim(dim), _, _, _)
      case WorkGroup => ParFor(level, dim, unroll, "block_id_")(
        blockId(dim), _, gridDim(dim), _, _, _)
      case Warp =>      ParFor(level, dim, unroll, "warp_id_")(
        warpId(dim), _, warpDim(dim), _, _, _)
      case Lane =>      ParFor(level, dim, unroll, "lane_id_")(
        laneId(dim), _, warpSize, _, _, _)
      case Sequential => throw new Exception("This should not happen")
    }
}
