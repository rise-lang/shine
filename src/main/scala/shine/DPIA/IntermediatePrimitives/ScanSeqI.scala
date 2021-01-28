package shine.DPIA.IntermediatePrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.acc
import shine.DPIA.DSL._
import shine.DPIA.FunctionalPrimitives.{NatAsIndex, Transmute}
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.IndexData
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


object ScanSeqInclusiveI {
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
         ( (out `@` Transmute(read, int, IndexType(n + 1), 0)) :=| dt2 | accumulator.rd ) `;`
          `for`(n, i =>
            f(in `@` i)(accumulator.rd)(accumulator.wr) `;`
              //FIXME remove general assignment
              {
                val idx = Transmute(read, int, IndexType(n + 1), Transmute(read, IndexType(n + 1), int, i) + 1)
                (out `@` idx) :=| dt2 | accumulator.rd
              }
          )
      )
  }
}
