package lift.OpenMP

import lift.OpenMP.primitives._
import lift.core.TypedDSL._

object TypedDSL {
  def mapPar: TDSL[MapPar] = toTDSL(MapPar()())
  def reducePar: TDSL[ReducePar] = toTDSL(ReducePar()())
}
