package shine.cuda

import shine.DPIA.ImperativePrimitives.Assign
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{AccType, ArrayType, CommType, DataType, ExpType, WmmaFragment}

class TranslationContext() extends shine.OpenCL.TranslationContext {
  override def assign(dt: DataType,
                      lhs: Phrase[AccType],
                      rhs: Phrase[ExpType]): Phrase[CommType] = {
    dt match {
      //TODO This is wrong and should not happen!!!
      case _: WmmaFragment =>
        Assign(dt, lhs, rhs)

//      case ArrayType(4, )

      case _ =>
        super.assign(dt, lhs, rhs)
    }
  }
}
