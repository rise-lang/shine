package ExpPatterns

import Core._

case class ToGlobal(dt1: DataType,
                    dt2: DataType,
                    f: Phrase[ExpType -> ExpType],
                    input: Phrase[ExpType])
  extends To(dt1, dt2, f, input, GlobalMemory, ToGlobal)
