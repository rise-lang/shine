package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.IntermediatePrimitives.ReduceSeqI
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._

final case class ReduceSeq(n: Nat,
                           dt1: DataType,
                           dt2: DataType,
                           f: Phrase[ExpType -> (ExpType -> ExpType)],
                           init: Phrase[ExpType],
                           array: Phrase[ExpType])
  extends AbstractReduce(n, dt1, dt2, f, init, array)
{
  override def makeReduce = ReduceSeq
  override def makeReduceI = ReduceSeqI.apply
}

