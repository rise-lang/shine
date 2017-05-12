package idealised.OpenCL.FunctionalPrimitives

import idealised.Core._
import idealised.FunctionalPrimitives.AbstractMap
import idealised.OpenCL.IntermediatePrimitives.MapWorkGroupI

final case class MapWorkGroup(n: Nat,
                              dt1: DataType,
                              dt2: DataType,
                              f: Phrase[ExpType -> ExpType],
                              array: Phrase[ExpType])
  extends AbstractMap(n, dt1, dt2, f, array) {
  override def makeMap = MapWorkGroup

  override def makeMapI = MapWorkGroupI
}
