package CommandPatterns

import Core.PhraseType._
import Core._
import DSL._
import Rewriting.SubstituteImplementations

case class MapLocalI(out: Phrase[AccType],
                     f: Phrase[AccType -> (ExpType -> CommandType)],
                     in: Phrase[ExpType]) extends AbstractMapI(out, f, in) {

  override def makeMapI = MapLocalI

  override def substituteImpl: Phrase[CommandType] = {
    // TODO: replace with for loop iterating over local stuff
    `for`(length(in), i => {
      SubstituteImplementations( f(out `@` i)(in `@` i) )
    })
  }

}
