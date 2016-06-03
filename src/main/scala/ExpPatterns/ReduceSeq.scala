package ExpPatterns

import CommandPatterns.{ReduceIAcc, ReduceIExp}
import Core.PhraseType._
import Core._

case class ReduceSeq(f: Phrase[ExpType -> (ExpType -> ExpType)],
                  init: Phrase[ExpType],
                  array: Phrase[ExpType]) extends AbstractReduce(f, init, array, ReduceSeq, ReduceIAcc, ReduceIExp)