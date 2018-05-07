package idealised.OpenMP.FunctionalPrimitives

import idealised.DPIA.FunctionalPrimitives.AbstractMap
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{DataType, ExpType}
import idealised.DPIA._
import idealised.OpenMP.IntermediatePrimitives.MapParI

final case class MapPar(n: Nat,
                        dt1: DataType,
                        dt2: DataType,
                        f: Phrase[ExpType -> ExpType],
                        array: Phrase[ExpType])
  extends AbstractMap(n, dt1, dt2, f, array) {
  override def makeMap = MapPar

  override def makeMapI = MapParI
}
