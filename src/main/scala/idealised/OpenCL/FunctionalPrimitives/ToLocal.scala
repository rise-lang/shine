package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL

final case class ToLocal(dt: DataType,
                         input: Phrase[ExpType])
  extends To(Local, dt, input, ToLocal)
