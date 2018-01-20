package idealised.OpenMP.FunctionalPrimitives

import idealised.DPIA.FunctionalPrimitives.AbstractMap
import idealised.OpenMP.IntermediatePrimitives.MapSeqI
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{DataType, ExpType}
import idealised.DPIA._

final case class MapSeq(n: Nat,
                        dt1: DataType,
                        dt2: DataType,
                        f: Phrase[ExpType -> ExpType],
                        array: Phrase[ExpType])
  extends AbstractMap(n, dt1, dt2, f, array) {
  override def makeMap = MapSeq

  override def makeMapI = MapSeqI
}
