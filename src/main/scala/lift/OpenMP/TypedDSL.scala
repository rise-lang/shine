package lift.OpenMP

import lift.OpenMP.primitives._
import lift.core.TypedDSL._

object TypedDSL {
  def mapPar: TDSL[MapPar] = tdsl(MapPar()())
  def reducePar: TDSL[ReducePar] = tdsl(ReducePar()())
}
