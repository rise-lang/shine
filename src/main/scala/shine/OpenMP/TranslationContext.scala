package shine.OpenMP
import shine.DPIA.DSL.λ
import shine.DPIA.IntermediatePrimitives.MapVecI
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType, VectorType, read}

class TranslationContext() extends shine.C.TranslationContext {
  override def assign(dt: DataType,
                      A: Phrase[AccType],
                      E: Phrase[ExpType]): Phrase[CommType] = {
    dt match {
      case VectorType(n, st) =>
        MapVecI(n, st, st, λ(ExpType(st, read))(x => λ(AccType(st))(a => assign(st, a, x) )), E, A)

      case _ => super.assign(dt, A, E)
    }
  }
}
