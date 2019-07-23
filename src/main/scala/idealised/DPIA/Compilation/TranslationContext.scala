package idealised.DPIA.Compilation

import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommType, DataType, ExpType}

trait TranslationContext {

  def assign(dt: DataType,
             lhs: Phrase[AccType],
             rhs: Phrase[ExpType]): Phrase[CommType]

}
