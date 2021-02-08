package shine.cuda.primitives.imperative

import shine.DPIA.Phrases.{CommandPrimitive, _}
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL._
import shine.cuda.{blockDim, blockId, globalDim, globalId, gridDim, laneId, threadId, warpDim, warpId, warpSize}
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class ParFor(level: ParallelismLevel,
                        dim: Char,
                        unroll: Boolean)
                       (val n: Nat,
                        val dt: DataType,
                        val out: Phrase[AccType],
                        val loopBody: Phrase[ExpType ->: AccType ->: CommType],
                        val init: Nat = ParFor.initInit(level, dim),
                        val step: Nat = ParFor.initStep(level, dim)
                       ) extends CommandPrimitive {
  val name: String = ParFor.initName(level)

  out :: accT(n`.`dt)
  loopBody :: expT(idx(n), read) ->: accT(dt) ->: comm

  lazy val unwrapBody: (Identifier[ExpType], Identifier[AccType], Phrase[CommType]) = loopBody match {
    case Lambda(i, Lambda(o, body)) => (i, o, body)
    case _ => throw new Exception("This should not happen")
  }
}

object ParFor {
  def initInit(level: ParallelismLevel, dim: Char): Nat = level match {
    case Global     => globalId(dim)
    case Local      => threadId(dim)
    case WorkGroup  => blockId(dim)
    case Warp       => warpId(dim)
    case Lane       => laneId(dim)
    case Sequential => throw new Exception("This should not happen")
  }

  def initStep(level: ParallelismLevel, dim: Char): Nat = level match {
    case Global     => globalDim(dim)
    case Local      => blockDim(dim)
    case WorkGroup  => gridDim(dim)
    case Warp       => warpDim(dim)
    case Lane       => warpSize
    case Sequential => throw new Exception("This should not happen")
  }

  def initName(level: ParallelismLevel): String = level match {
    case Global     => freshName("gl_id_")
    case Local      => freshName("tid_")
    case WorkGroup  => freshName("block_id_")
    case Warp       => freshName("warp_id_")
    case Lane       => freshName(s"lane_id_")
    case Sequential => throw new Exception("This should not happen")
  }
}
