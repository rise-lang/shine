package rise.openMP

import rise.core.Primitive

object DSL {
  def mapPar: Primitive = primitives.mapPar.primitive
  def reducePar: Primitive = primitives.reducePar.primitive
}
