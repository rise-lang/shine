package shine.OpenMP

import shine.C
import shine.DPIA.DSL.{ExpPhraseExtensions, fun}
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{AccType, CommType, ExpType}
import rise.core.types.{DataType, read}
import rise.core.types.DataType.VectorType

class TranslationContext() extends C.Compilation.TranslationContext {
  override def assign(dt: DataType,
                      A: Phrase[AccType],
                      E: Phrase[ExpType]): Phrase[CommType] = {
    dt match {
      case VectorType(n, st) =>
        shine.OpenMP.DSL.parForVec(n, st, A, i => a => assign(st, a, E `@v` i))

      case _ => super.assign(dt, A, E)
    }
  }
}
