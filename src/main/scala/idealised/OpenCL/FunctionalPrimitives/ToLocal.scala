package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.AddressSpace.Local
import idealised.DPIA.Types._

final case class ToLocal(dt: DataType,
                         input: Phrase[ExpType])
  extends To(Local, dt, input, ToLocal)
