package HighLevelCombinators

import Core._
import MidLevelCombinators.{ReduceIAcc, ReduceIExp}

case class ReduceSeq(n: Nat,
                     dt1: DataType, dt2: DataType,
                     f: Phrase[ExpType -> (ExpType -> ExpType)],
                     init: Phrase[ExpType],
                     array: Phrase[ExpType])
  extends AbstractReduce(n, dt1, dt2, f, init, array, ReduceSeq, ReduceIAcc, ReduceIExp)