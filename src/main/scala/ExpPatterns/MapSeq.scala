package ExpPatterns

import CommandPatterns.MapSeqI
import Core._
import Core.PhraseType._

case class MapSeq(f: Phrase[ExpType -> ExpType], array: Phrase[ExpType]) extends AbstractMap(f, array) {

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): ExpPattern = {
    MapSeq(
      OperationalSemantics.substitute(phrase, `for`, f),
      OperationalSemantics.substitute(phrase, `for`, array))
  }

  override def toC: String = ???

  override def prettyPrint: String = s"(mapSeq ${PrettyPrinter(f)} ${PrettyPrinter(array)})"

  override def makeMapI = MapSeqI

}
