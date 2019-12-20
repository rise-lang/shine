package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.IntermediatePrimitives.MapSeqI
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._

final case class MapSeq(override val n: Nat,
                        override val dt1: DataType,
                        override val dt2: DataType,
                        override val f: Phrase[ExpType ->: ExpType],
                        override val array: Phrase[ExpType])
  extends AbstractMapLoop(n, dt1, dt2, f, array)
{
  override def makeMap = MapSeq

  override def makeMapI(n: Nat, dt1: DataType, dt2: DataType,
                        f: Phrase[->:[ExpType, ->:[AccType, CommType]]],
                        array: Phrase[ExpType],
                        out: Phrase[AccType])
                       (implicit context: TranslationContext): Phrase[CommType] =
    MapSeqI(n, dt1, dt2, f, array, out)
}
