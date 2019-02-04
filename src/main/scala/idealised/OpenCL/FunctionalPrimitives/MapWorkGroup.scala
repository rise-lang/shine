package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.FunctionalPrimitives.AbstractMapLoop
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{DataType, ExpType}
import idealised.DPIA._
import idealised.OpenCL.IntermediatePrimitives.MapWorkGroupI

final case class MapWorkGroup(dim: Int)(n: Nat,
                                        dt1: DataType,
                                        dt2: DataType,
                                        f: Phrase[ExpType -> ExpType],
                                        array: Phrase[ExpType])
  extends AbstractMapLoop(n, dt1, dt2, f, array)
{
  override def makeMap = MapWorkGroup(dim)
  override def makeMapI = MapWorkGroupI(dim).apply
}
