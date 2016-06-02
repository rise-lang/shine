package CommandPatterns

import Core.PhraseType._
import Core._
import DSL._
import Rewriting.SubstituteImplementations

case class MapSeqI(out: Phrase[AccType],
                   f: Phrase[AccType -> (ExpType -> CommandType)],
                   in: Phrase[ExpType]) extends AbstractMapI(out, f, in) {

  override def makeMapI = MapSeqI

  override def substituteImpl: Phrase[CommandType] = {
    `for`(length(in), i => {
      SubstituteImplementations( f(out `@` i)(in `@` i) )
    })
  }

}
