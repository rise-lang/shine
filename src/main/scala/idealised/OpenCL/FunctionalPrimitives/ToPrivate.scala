package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL

final case class ToPrivate(dt: DataType,
                           input: Phrase[ExpType])
  extends To(PrivateMem, dt, input, ToPrivate)
