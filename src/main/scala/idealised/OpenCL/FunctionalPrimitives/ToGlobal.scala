package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.AddressSpace.Global
import idealised.DPIA.Types._

final case class ToGlobal(dt: DataType,
                          input: Phrase[ExpType])
  extends To(Global, dt, input, ToGlobal)
