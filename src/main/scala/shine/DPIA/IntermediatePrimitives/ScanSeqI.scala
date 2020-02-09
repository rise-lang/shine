package shine.DPIA.IntermediatePrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._

import scala.language.reflectiveCalls

object ScanSeqI {
  def apply(n: Nat, dt1: DataType, dt2: DataType,
            f: Phrase[ExpType ->: ExpType ->: AccType ->: CommType],
            init: Phrase[ExpType],
            in: Phrase[ExpType],
            out: Phrase[AccType])
           (implicit context: TranslationContext): Phrase[CommType] =
  {
    `new`(dt2, acc =>
      (acc.wr :=| dt2 | init) `;`
        `for`(n, i =>
          f(in `@` i)(acc.rd)(acc.wr) `;`
            //FIXME remove general assignment
            ((out `@` i) :=| dt2 | acc.rd)
        )
    )
  }
}
