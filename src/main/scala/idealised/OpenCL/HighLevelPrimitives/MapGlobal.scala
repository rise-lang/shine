package idealised.OpenCL.HighLevelPrimitives

import idealised.Core._
import idealised.FunctionalPrimitives.AbstractMap
import idealised.OpenCL.MidLevelPrimitives.MapGlobalI

final case class MapGlobal(n: Nat,
                           dt1: DataType,
                           dt2: DataType,
                           f: Phrase[ExpType -> ExpType],
                           array: Phrase[ExpType])
  extends AbstractMap(n, dt1, dt2, f, array) {
  override def makeMap = MapGlobal

  override def makeMapI = MapGlobalI
}
