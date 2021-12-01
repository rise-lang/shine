package shine.DPIA.Compilation

import rise.core.types.DataType
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{AccType, CommType, ExpType}

trait TranslationContext {

  def assign(dt: DataType,
             lhs: Phrase[AccType],
             rhs: Phrase[ExpType]): Phrase[CommType]

}
