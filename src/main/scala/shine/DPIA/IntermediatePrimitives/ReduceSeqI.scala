package shine.DPIA.IntermediatePrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.acc
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._

object ReduceSeqI {
  def apply(n: Nat, dt1: DataType, dt2: DataType,
            f: Phrase[ExpType ->: ExpType ->: AccType ->: CommType],
            init: Phrase[ExpType],
            in: Phrase[ExpType],
            out: Phrase[ExpType ->: CommType],
            unroll: Boolean = false)
           (implicit context: TranslationContext): Phrase[CommType] =
  {
    comment("reduceSeq")`;`
    `new`(dt2, accum =>
      acc(init)(accum.wr) `;`
        `for`(n, i => f(accum.rd)(in `@` i)(accum.wr), unroll) `;`
        out(accum.rd)
    )
  }
}
