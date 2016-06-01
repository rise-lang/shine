package CommandPatterns

import Core.PhraseType._
import Core._
import DSL._

case class MapWorkgroupI(out: Phrase[AccType],
                         f: Phrase[AccType -> (ExpType -> CommandType)],
                         in: Phrase[ExpType]) extends AbstractMapI(out, f, in) {

  override def makeMapI = MapWorkgroupI

  override def substituteImpl: Phrase[CommandType] = {
    // TODO: replace with for loop iterating over workgroup stuff
    `for`(length(in), i => {
      f(out `@` i)(in `@` i)
    })
  }

}
