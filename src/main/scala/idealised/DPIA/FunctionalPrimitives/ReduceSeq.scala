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
  extends AbstractReduce(n, dt1, dt2, f, init, array) {
  override def makeReduce: (Nat, DataType, DataType, Phrase[ExpType -> (ExpType -> ExpType)], Phrase[ExpType], Phrase[ExpType]) => ReduceSeq = ReduceSeq

  override def makeReduceI: (Nat, DataType, DataType, Phrase[ExpType -> (ExpType -> (AccType -> CommandType))], Phrase[ExpType], Phrase[ExpType], Phrase[ExpType -> CommandType]) => ReduceSeqI = ReduceSeqI
}

