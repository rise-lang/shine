package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.primitives.intermediate.MapSeqI
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._

final case class MapSeqUnroll(override val n: Nat,
                              override val dt1: DataType,
                              override val dt2: DataType,
                              override val f: Phrase[ExpType ->: ExpType],
                              override val array: Phrase[ExpType])
  extends AbstractMapLoop(n, dt1, dt2, f, array)
{
  override def makeMap = MapSeqUnroll

  override def makeMapI(n: Nat, dt1: DataType, dt2: DataType,
                        f: Phrase[->:[ExpType, ->:[AccType, CommType]]],
                        array: Phrase[ExpType],
                        out: Phrase[AccType])
                       (implicit context: TranslationContext): Phrase[CommType] =
    MapSeqI(n, dt1, dt2, f, array, out, unroll = true)
}
