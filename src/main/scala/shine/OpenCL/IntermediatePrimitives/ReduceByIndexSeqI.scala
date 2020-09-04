package shine.OpenCL.IntermediatePrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.acc
import shine.DPIA.DSL.{`new` => _, _}
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.AdjustArraySizesForAllocations
import shine.OpenCL.DSL._

object ReduceByIndexSeqI {
  def apply(n: Nat,
            k: Nat,
            dt: DataType,
            f: Phrase[ExpType ->: ExpType ->: AccType ->: CommType],
            hist: Phrase[ExpType],
            is: Phrase[ExpType],
            xs: Phrase[ExpType],
            out: Phrase[ExpType ->: CommType])
           (implicit context: TranslationContext): Phrase[CommType] = {
    val adj = AdjustArraySizesForAllocations(hist, ArrayType(k, dt), AddressSpace.Global)

    comment("reduceByIndexSeq") `;`
      `new` (AddressSpace.Global) (adj.dt, accumulator =>
        `for`(k, j => acc(hist `@` j)(adj.accF(accumulator.wr) `@` j)) `;`
          `for`(n, j =>
            `new` (AddressSpace.Private) (int, i =>
              acc(is `@` j)(i.wr) `;`
                //TODO: Figure out how to use i instead of j here
                f(adj.exprF(accumulator.rd) `@` j)(xs `@` j)(adj.accF(accumulator.wr) `@` j))) `;`
          out(adj.exprF(accumulator.rd))
      )
  }
}
