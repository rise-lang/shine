package shine.DPIA.primitives.intermediate

import rise.core.types.DataType
import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.acc
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
    comment("scanSeq")`;`
    `new`(dt2, accumulator =>
      acc(init)(accumulator.wr) `;`
        `for`(n, i =>
          f(in `@` i)(accumulator.rd)(accumulator.wr) `;`
            //FIXME remove general assignment
            ((out `@` i) :=| dt2 | accumulator.rd)
        )
    )
  }
}
