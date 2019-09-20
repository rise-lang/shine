package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.FunctionalPrimitives.AbstractMapLoop
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.IntermediatePrimitives.MapWorkGroupI

final case class MapWorkGroup(dim: Int)(override val n: Nat,
                                        override val dt1: DataType,
                                        override val dt2: DataType,
                                        override val f: Phrase[ExpType ->: ExpType],
                                        override val array: Phrase[ExpType])
  extends AbstractMapLoop(n, dt1, dt2, f, array)
{
  override def makeMap = MapWorkGroup(dim)

  override def makeMapI(n: Nat, dt1: DataType, dt2: DataType,
                        f: Phrase[->:[ExpType, ->:[AccType, CommType]]],
                        array: Phrase[ExpType],
                        out: Phrase[AccType])
                       (implicit context: TranslationContext): Phrase[CommType] =
    MapWorkGroupI(dim)(n, dt1, dt2, f, array, out)
}
