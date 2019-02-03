package idealised.OpenCL

import idealised.DPIA.ImperativePrimitives.Assign
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType, VectorType}

class TranslationContext() extends idealised.C.TranslationContext {
  override def assign(dt: DataType,
                      lhs: Phrase[AccType],
                      rhs: Phrase[ExpType]): Phrase[CommandType] = {
    dt match {
      case VectorType(_, _) => Assign(dt, lhs, rhs)
      case _ => super.assign(dt, lhs, rhs)
    }
  }
}