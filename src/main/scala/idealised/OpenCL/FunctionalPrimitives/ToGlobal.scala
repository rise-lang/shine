package idealised.OpenCL.FunctionalPrimitives

import idealised._
import idealised.Core._

final case class ToGlobal(dt1: DataType,
                          dt2: DataType,
                          f: Phrase[ExpType -> ExpType],
                          input: Phrase[ExpType])
  extends To(dt1, dt2, f, input, OpenCL.GlobalMemory, ToGlobal)
