package ExpPatterns

import Core._
import Core.PhraseType._

case class MapWorkgroup(f: Phrase[ExpType -> ExpType], array: Phrase[ExpType]) extends AbstractMap(f, array) {

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): ExpPattern = {
    MapWorkgroup(
      OperationalSemantics.substitute(phrase, `for`, f),
      OperationalSemantics.substitute(phrase, `for`, array))
  }

  override def toC: String = ???

  override def prettyPrint: String = s"(mapWrg ${PrettyPrinter(f)} ${PrettyPrinter(array)})"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = ???

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = ???
}
