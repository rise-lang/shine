package ExpPatterns

import CommandPatterns.MapWorkgroupI
import Core._
import Core.PhraseType._

case class MapWorkgroup(f: Phrase[ExpType -> ExpType], array: Phrase[ExpType]) extends AbstractMap(f, array, MapWorkgroup, MapWorkgroupI)
