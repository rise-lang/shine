package shine.OpenCL.IntermediatePrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.acc
import shine.DPIA.DSL.{`new` => _, _}
import shine.DPIA.FunctionalPrimitives.NatAsIndex
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
        //TODO: Is there a better way than copying the whole input histogram?
        `for`(k, j => acc(hist `@` j)(adj.accF(accumulator.wr) `@` j)) `;`
          `for`(n, j =>
            `new` (AddressSpace.Private) (NatType, i =>
              acc(is `@` j)(i.wr) `;`
              `if` (i.rd `<` Natural(k),
                    //TODO: Using i as an index here always throws an key not found error
                    //      (=> 3 global memory accesses needed instead of 1)
                    f(adj.exprF(accumulator.rd) `@` NatAsIndex(k, is `@` j))
                     (xs `@` j)
                     (adj.accF(accumulator.wr) `@` NatAsIndex(k, is `@` j)),
                    //TODO: Is there a NOP for the else phrase or can you leave it out completely?
                    acc(Natural(0))(i.wr)))) `;`
          out(adj.exprF(accumulator.rd))
      )
  }
}
