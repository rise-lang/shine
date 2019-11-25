package lift.OpenMP

import lift.core.DSL._
import lift.core.{Expr, primitives => core}
import lift.OpenMP.primitives._

object DSL {
  def mapPar: MapPar = MapPar()()
  def reducePar: ReducePar = ReducePar()()
}
