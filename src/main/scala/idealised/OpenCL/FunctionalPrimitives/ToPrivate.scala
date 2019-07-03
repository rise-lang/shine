package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.AddressSpace.Private
import idealised.DPIA.Types._

final case class ToPrivate(dt: DataType,
                           input: Phrase[ExpType])
  extends To(Private, dt, input, ToPrivate)
