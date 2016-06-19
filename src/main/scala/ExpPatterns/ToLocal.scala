package ExpPatterns

import Core.PhraseType.->
import Core._

case class ToLocal(f: Phrase[ExpType -> ExpType],
                   input: Phrase[ExpType])
  extends To(f, input, LocalMemory, ToLocal)
