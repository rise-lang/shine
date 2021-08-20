package shine.OpenMP

import shine.C
import shine.DPIA.DSL.fun
import shine.DPIA.primitives.intermediate.MapVecI
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
        MapVecI(n, st, st, fun(ExpType(st, read))(x => fun(AccType(st))(a => assign(st, a, x) )), E, A)

      case _ => super.assign(dt, A, E)
    }
  }
}
