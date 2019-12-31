package rise.OpenMP

import rise.OpenMP.primitives._

object DSL {
  def mapPar: MapPar = MapPar()()
  def reducePar: ReducePar = ReducePar()()
}
