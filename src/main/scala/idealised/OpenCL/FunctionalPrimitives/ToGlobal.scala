package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised._

final case class ToGlobal(dt: DataType,
                          input: Phrase[ExpType])
  extends To(GlobalMem, dt, input, ToGlobal)
