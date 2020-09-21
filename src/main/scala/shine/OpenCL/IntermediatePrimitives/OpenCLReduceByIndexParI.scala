package shine.OpenCL.IntermediatePrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.acc
import shine.DPIA.DSL.{`new` => _, _}
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.AdjustArraySizesForAllocations
import shine.OpenCL.DSL._

object OpenCLReduceByIndexParI {
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

    comment("reduceByIndexPar") `;`
      `new` (histAddrSpace) (adj.dt, accumulator =>
        acc(hist)(adj.accF(accumulator.wr)) `;`

          `for`(n, j =>
            atomicBinOp(dt, f,
              adj.accF(accumulator.wr) `@` fst(input `@` j),
              snd(input `@` j))
          ) `;`

          out(adj.exprF(accumulator.rd))
      )
  }
}
