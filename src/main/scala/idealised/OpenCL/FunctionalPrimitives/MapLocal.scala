package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.FunctionalPrimitives.AbstractMapLoop
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.IntermediatePrimitives.MapLocalI

final case class MapLocal(dim: Int)(n: Nat,
                                    dt1: DataType,
                                    dt2: DataType,
                                    f: Phrase[ExpType -> ExpType],
                                    array: Phrase[ExpType])
  extends AbstractMapLoop(n, dt1, dt2, f, array)
{
  override def makeMap = MapLocal(dim)
  override def makeMapI(n: Nat, dt1: DataType, dt2: DataType,
                        f: Phrase[->[ExpType, ->[AccType, CommType]]],
                        array: Phrase[ExpType],
                        out: Phrase[AccType])
                       (implicit context: TranslationContext): Phrase[CommType] =
    MapLocalI(dim)(n, dt1, dt2, f, array, out)
}
