package shine.OpenCL.Compilation

import rise.core.types.{DataType, VectorType}
import shine.C
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{AccType, CommType, ExpType}
import shine.DPIA.primitives.imperative.Assign

class TranslationContext() extends C.Compilation.TranslationContext {
  override def assign(dt: DataType,
                      lhs: Phrase[AccType],
                      rhs: Phrase[ExpType]): Phrase[CommType] = {
    dt match {
      case VectorType(_, _) => Assign(dt, lhs, rhs)
      case _ => super.assign(dt, lhs, rhs)
    }
  }
}