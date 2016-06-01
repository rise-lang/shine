package ExpPatterns

import CommandPatterns.MapGlobalI
import Core._
import Core.PhraseType._

case class MapGlobal(f: Phrase[ExpType -> ExpType], array: Phrase[ExpType]) extends AbstractMap(f, array) {

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): ExpPattern = {
    MapGlobal(
      OperationalSemantics.substitute(phrase, `for`, f),
      OperationalSemantics.substitute(phrase, `for`, array))
  }

  override def toC: String = ???

  override def prettyPrint: String = s"(mapGlb ${PrettyPrinter(f)} ${PrettyPrinter(array)})"

  override def makeMapI = MapGlobalI
}
