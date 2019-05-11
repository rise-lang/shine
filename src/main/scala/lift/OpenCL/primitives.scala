package lift.OpenCL

import lift.core.DSL._
import lift.core.types._
import lift.core.{Expr, primitives => core}

import scala.language.implicitConversions

object primitives {
  sealed trait Primitive extends lift.core.Primitive

  case class mapGlobal(dim: Int) extends Primitive {
    override def t: Type = core.map.t
  }

  object mapGlobal {
    def apply(): mapGlobal = mapGlobal(0)
    def apply(e: Expr): Expr = mapGlobal(0)(e)

    implicit def toMapGlobal(m: mapGlobal.type): mapGlobal = mapGlobal(0)
  }

}
