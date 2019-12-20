package shine.DPIA.Compilation

import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType}

trait TranslationContext {

  def assign(dt: DataType,
             lhs: Phrase[AccType],
             rhs: Phrase[ExpType]): Phrase[CommType]

}
