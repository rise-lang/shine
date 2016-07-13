package OpenCL.HighLevelCombinators

import Core.{->, DataType, ExpType, Nat, Phrase}
import HighLevelCombinators.AbstractReduce
import MidLevelCombinators.{ReduceIAcc, ReduceIExp}

final case class ReduceSeq(n: Nat,
                           dt1: DataType, dt2: DataType,
                           f: Phrase[ExpType -> (ExpType -> ExpType)],
                           init: Phrase[ExpType],
                           array: Phrase[ExpType])
  extends AbstractReduce(n, dt1, dt2, f, init, array) {
  override def makeReduce = ReduceSeq

  override def makeReduceIExp = ReduceIExp

  override def makeReduceIAcc = ReduceIAcc
}
