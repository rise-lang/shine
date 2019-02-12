package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.FunctionalPrimitives.AbstractMapLoop
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.IntermediatePrimitives.MapGlobalI

final case class MapGlobal(dim: Int)(n: Nat,
                                     dt1: DataType,
                                     dt2: DataType,
                                     f: Phrase[ExpType -> ExpType],
                                     array: Phrase[ExpType])
  extends AbstractMapLoop(n, dt1, dt2, f, array)
{
  override def makeMap = MapGlobal(dim)
  override def makeMapI(n: Nat, dt1: DataType, dt2: DataType,
                        f: Phrase[->[ExpType, ->[AccType, CommandType]]],
                        array: Phrase[ExpType],
                        out: Phrase[AccType])
                       (implicit context: TranslationContext): Phrase[CommandType] =
    MapGlobalI(dim)(n, dt1, dt2, f, array, out)
}
