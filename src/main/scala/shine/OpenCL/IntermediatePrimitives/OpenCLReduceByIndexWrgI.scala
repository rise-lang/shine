package shine.OpenCL.IntermediatePrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.acc
import shine.DPIA.DSL.{`new` => _, _}
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType.idx
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.AdjustArraySizesForAllocations
import shine.OpenCL.DSL._
import shine.OpenCL.ImperativePrimitives.StridedForLocal

object OpenCLReduceByIndexWrgI {
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

    comment("oclReduceByIndexLocal") `;`
      `new` (histAddrSpace) (adj.dt, accumulator =>
        acc(hist)(adj.accF(accumulator.wr)) `;`

            StridedForLocal(0)(n,
              λ(expT(idx(n), read))(j =>
                atomicBinOpAssign(dt, histAddrSpace, f,
                  adj.accF(accumulator.wr) `@` fst(input `@` j),
                  snd(input `@` j))
              )) `;`

          barrier() `;`

          out(adj.exprF(accumulator.rd))
      )
  }
}
