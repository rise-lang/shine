package ExpPatterns

import CommandPatterns.MapGlobalI
import Core._
import Core.PhraseType._

case class MapGlobal(f: Phrase[ExpType -> ExpType], array: Phrase[ExpType]) extends AbstractMap(f, array, MapGlobal, MapGlobalI)
