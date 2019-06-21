package lift.OpenMP

import lift.core.types._
import lift.core.{primitives => core}

object primitives {
  sealed trait Primitive extends lift.core.Primitive

  case object mapPar extends Primitive {
    override def t: Type = core.map.t
  }

  case object reducePar extends Primitive {
    override def t: Type = core.reduce.t
  }
}