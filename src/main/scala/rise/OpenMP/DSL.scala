package rise.OpenMP

import rise.core.DSL._
import rise.core.{Expr, primitives => core}
import rise.OpenMP.primitives._

object DSL {
  def mapPar: MapPar = MapPar()()
  def reducePar: ReducePar = ReducePar()()
}
