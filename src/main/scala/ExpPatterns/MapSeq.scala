package ExpPatterns

import CommandPatterns.MapSeqI
import Core._
import Core.PhraseType._

case class MapSeq(f: Phrase[ExpType -> ExpType], array: Phrase[ExpType]) extends AbstractMap(f, array, MapSeq, MapSeqI)
