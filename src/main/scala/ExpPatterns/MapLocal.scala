package ExpPatterns

import CommandPatterns.MapLocalI
import Core._
import Core.PhraseType._

case class MapLocal(f: Phrase[ExpType -> ExpType], array: Phrase[ExpType]) extends AbstractMap(f, array) {

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): ExpPattern = {
    MapLocal(
      OperationalSemantics.substitute(phrase, `for`, f),
      OperationalSemantics.substitute(phrase, `for`, array))
  }

  override def toC: String = ???

  override def prettyPrint: String = s"(mapLcl ${PrettyPrinter(f)} ${PrettyPrinter(array)})"

  override def makeMapI = MapLocalI
}
