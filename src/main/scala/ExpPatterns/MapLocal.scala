package ExpPatterns

import CommandPatterns.MapLocalI
import Core._
import Core.PhraseType._

case class MapLocal(f: Phrase[ExpType -> ExpType], array: Phrase[ExpType]) extends AbstractMap(f, array, MapLocal, MapLocalI)
