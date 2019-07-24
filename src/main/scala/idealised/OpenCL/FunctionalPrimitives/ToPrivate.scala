package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL

final case class ToPrivate(dt1: DataType,
                           dt2: DataType,
                           f: Phrase[ExpType ->: ExpType],
                           input: Phrase[ExpType])
  extends To(dt1, dt2, f, input, OpenCL.PrivateMemory, ToPrivate)
