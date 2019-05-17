package lift.OpenMP

import lift.core.types._
import lift.core.{primitives => cp}

object primitives {
  sealed trait Primitive extends lift.core.Primitive

  case object mapPar extends Primitive {
    override def t: Type = cp.map.t
  }

  case object reducePar extends Primitive {
    override def t: Type = cp.reduce.t
  }
}