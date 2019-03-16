package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.IntermediatePrimitives.ReduceSeqI
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._

final case class ReduceSeqUnroll(n: Nat,
                                 dt1: DataType,
                                 dt2: DataType,
                                 f: Phrase[ExpType -> (ExpType -> ExpType)],
                                 init: Phrase[ExpType],
                                 array: Phrase[ExpType])
  extends AbstractReduce(n, dt1, dt2, f, init, array)
{
  override def makeReduce = ReduceSeqUnroll

  override def makeReduceI(n: Nat,
                           dt1: DataType,
                           dt2: DataType,
                           f: Phrase[->[ExpType, ->[ExpType, ->[AccType, CommandType]]]],
                           init: Phrase[ExpType],
                           array: Phrase[ExpType],
                           out: Phrase[->[ExpType, CommandType]])
                          (implicit context: TranslationContext): Phrase[CommandType] =
    ReduceSeqI(n, dt1, dt2, f, init, array, out, unroll = true)
}
