package ExpPatterns

import Core.PhraseType.->
import Core._

case class ToGlobal(f: Phrase[ExpType -> ExpType],
                   input: Phrase[ExpType])
  extends To(f, input, GlobalMemory, ToGlobal)
