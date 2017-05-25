package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised._

final case class ToGlobal(dt1: DataType,
                          dt2: DataType,
                          f: Phrase[ExpType -> ExpType],
                          input: Phrase[ExpType])
  extends To(dt1, dt2, f, input, OpenCL.GlobalMemory, ToGlobal)
