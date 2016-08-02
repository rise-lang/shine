package idealised.OpenCL.HighLevelCombinators

import idealised.Core._
import idealised.HighLevelCombinators.AbstractMap
import idealised.MidLevelCombinators.AbstractMapI
import idealised.OpenCL.MidLevelCombinators.MapLocalI

final case class MapLocal(n: Nat,
                          dt1: DataType,
                          dt2: DataType,
                          f: Phrase[ExpType -> ExpType],
                          array: Phrase[ExpType])
  extends AbstractMap(n, dt1, dt2, f, array) {
  override def makeMap = MapLocal

  override def makeMapI = MapLocalI
}
