package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.IntermediatePrimitives.ReduceSeqI
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._

final case class ReduceSeqUnroll(n: Nat,
                                 dt1: DataType,
                                 dt2: DataType,
                                 f: Phrase[ExpType ->: ExpType ->: ExpType],
                                 init: Phrase[ExpType],
                                 array: Phrase[ExpType])
  extends AbstractReduce(n, dt1, dt2, f, init, array)
{
  override def makeReduce = ReduceSeqUnroll

  override def makeReduceI(n: Nat,
                           dt1: DataType,
                           dt2: DataType,
                           f: Phrase[->:[ExpType, ->:[ExpType, ->:[AccType, CommType]]]],
                           init: Phrase[ExpType],
                           array: Phrase[ExpType],
                           out: Phrase[->:[ExpType, CommType]])
                          (implicit context: TranslationContext): Phrase[CommType] =
    ReduceSeqI(n, dt1, dt2, f, init, array, out, unroll = true)
}
