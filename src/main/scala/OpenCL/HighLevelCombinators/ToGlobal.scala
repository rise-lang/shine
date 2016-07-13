package OpenCL.HighLevelCombinators

import Core._
import OpenCL.Core.GlobalMemory

final case class ToGlobal(dt1: DataType,
                          dt2: DataType,
                          f: Phrase[ExpType -> ExpType],
                          input: Phrase[ExpType])
  extends To(dt1, dt2, f, input, GlobalMemory, ToGlobal)
