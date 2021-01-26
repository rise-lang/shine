package shine.DPIA.primitives.intermediate

import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._

object MapSeqI {
  def apply(unroll: Boolean = false)
           (n: Nat, dt1: DataType, dt2: DataType,
            f: Phrase[ExpType ->: AccType ->: CommType],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommType] =
  {
    comment("mapSeq")`;`
    `for`(n, i => f(in `@` i)(out `@` i), unroll)
  }
}
