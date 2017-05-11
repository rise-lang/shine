package idealised.OpenCL.HighLevelPrimitives

import idealised.Core._
import idealised.HighLevelPrimitives.AbstractMap
import idealised.OpenCL.MidLevelPrimitives.MapSeqI

final case class MapSeq(n: Nat,
                        dt1: DataType,
                        dt2: DataType,
                        f: Phrase[ExpType -> ExpType],
                        array: Phrase[ExpType])
  extends AbstractMap(n, dt1, dt2, f, array) {
  override def makeMap = MapSeq

  override def makeMapI = MapSeqI
}
