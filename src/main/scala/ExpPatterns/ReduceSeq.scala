package ExpPatterns

import Core.PhraseType._
import Core._

case class ReduceSeq(f: Phrase[ExpType -> (ExpType -> ExpType)],
                  init: Phrase[ExpType],
                  array: Phrase[ExpType]) extends AbstractReduce(f, init, array) {

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): ExpPattern = {
    ReduceSeq(
      OperationalSemantics.substitute(phrase, `for`, f),
      OperationalSemantics.substitute(phrase, `for`, init),
      OperationalSemantics.substitute(phrase, `for`, array))
  }

  override def toC = ???

  override def prettyPrint: String = s"(reduceSeq ${PrettyPrinter(f)} ${PrettyPrinter(init)} ${PrettyPrinter(array)})"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = ???

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = ???
}