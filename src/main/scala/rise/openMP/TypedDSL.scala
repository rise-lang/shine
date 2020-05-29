package rise.openMP

import rise.openMP.primitives._
import rise.core.TypedDSL._

object TypedDSL {
  def mapPar: TDSL[MapPar] = toTDSL(MapPar()())
  def reducePar: TDSL[ReducePar] = toTDSL(ReducePar()())
}
