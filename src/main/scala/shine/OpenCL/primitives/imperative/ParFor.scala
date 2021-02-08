package shine.OpenCL.primitives.imperative

import shine.DPIA.Phrases.{CommandPrimitive, _}
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL._
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class ParFor(level: ParallelismLevel,
                        dim: Int,
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
  def initInit(level: ParallelismLevel, dim: Int): Nat = level match {
    case Global     => get_global_id(dim)
    case Local      => get_local_id (dim)
    case WorkGroup  => get_group_id (dim)
    case Sequential | Warp | Lane => throw new Exception("This should not happen")
  }

  def initStep(level: ParallelismLevel, dim: Int): Nat = level match {
    case Global     => get_global_size(dim)
    case Local      => get_local_size (dim)
    case WorkGroup  => get_num_groups (dim)
    case Sequential | Warp | Lane => throw new Exception("This should not happen")
  }

  def initName(level: ParallelismLevel): String = level match {
    case Global     => freshName("gl_id_")
    case Local      => freshName( "l_id_")
    case WorkGroup  => freshName("wg_id_")
    case Sequential | Warp | Lane => throw new Exception("This should not happen")
  }
}
