package ExpPatterns

import Core.PhraseType._
import Core._

case class ReduceSeq(f: Phrase[ExpType -> (ExpType -> ExpType)],
                  init: Phrase[ExpType],
                  array: Phrase[ExpType]) extends AbstractReduce(f, init, array, ReduceSeq) {

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = ???

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = ???

}