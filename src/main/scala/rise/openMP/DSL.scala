package rise.openMP

import rise.openMP.primitives._

object DSL {
  def mapPar: MapPar = MapPar()()
  def reducePar: ReducePar = ReducePar()()
}
