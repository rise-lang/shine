package idealised.DPIA.IntermediatePrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.language.reflectiveCalls

object ReduceSeqI {
  def apply(n: Nat, dt1: DataType, dt2: DataType,
            f: Phrase[ExpType ->: ExpType ->: AccType ->: CommType],
            init: Phrase[ExpType],
            in: Phrase[ExpType],
            out: Phrase[ExpType ->: CommType],
            unroll: Boolean = false)
           (implicit context: TranslationContext): Phrase[CommType] =
  {
    `new`(dt2, acc =>
      (acc.wr :=|dt2| init) `;`
        `for`(n, i => f(acc.rd)(in `@` i)(acc.wr), unroll) `;`
        out(acc.rd)
    )
  }
}
