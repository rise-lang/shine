package shine.OpenCL.IntermediatePrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.acc
import shine.DPIA.DSL.{`new` => _, _}
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.AdjustArraySizesForAllocations
import shine.OpenCL.DSL._

object OpenCLReduceByIndexSeqI {
  def apply(n: Nat,
            k: Nat,
            histAddrSpace: shine.DPIA.Types.AddressSpace,
            dt: DataType,
            f: Phrase[ExpType ->: ExpType ->: AccType ->: CommType],
            hist: Phrase[ExpType],
            input: Phrase[ExpType],
            out: Phrase[ExpType ->: CommType])
           (implicit context: TranslationContext): Phrase[CommType] = {
    val adj = AdjustArraySizesForAllocations(hist, ArrayType(k, dt), histAddrSpace)

    comment("reduceByIndexSeq") `;`
      `new` (histAddrSpace) (adj.dt, accumulator =>
        acc(hist)(adj.accF(accumulator.wr)) `;`

          `for`(n, j =>
            //`new` (AddressSpace.Private) (IndexType(k), i =>
            //  acc(is `@` j)(i.wr) `;`
                //TODO: Using i as an index here always throws an key not found error
                //      (=> 2 global memory accesses needed instead of 1)
                f(adj.exprF(accumulator.rd) `@` fst(input `@` j))
                 (snd(input `@` j))
                 (adj.accF(accumulator.wr) `@` fst(input `@` j))
           //)
          ) `;`

          out(adj.exprF(accumulator.rd))
      )
  }
}
