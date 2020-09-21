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
import shine.OpenCL.ImperativePrimitives.ParForGlobal

object OpenCLReduceByIndexGlobalI {
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

    comment("oclReduceByIndexGlobal") `;`
      `new` (histAddrSpace) (adj.dt, accumulator =>
        acc(hist)(adj.accF(accumulator.wr)) `;`

          //TODO: The size of the accumulator must be equal to the number of loop iterations.
          //      However in this case you have iterate n times and write into an array with a size of k.
          //      Declaring a n-sized array and using it as the accumulator fixes this,
          //      but probably isn't the best solution to this problem.
          `new`(histAddrSpace)(ArrayType(n, dt), acc_fix =>
            ParForGlobal(0)(n, dt, acc_fix.wr,
              λ(expT(idx(n), read))(j => λ(accT(dt))(a =>
                atomicBinOp(dt, f,
                  adj.accF(accumulator.wr) `@` fst(input `@` j),
                  snd(input `@` j))
              )))
          ) `;`

          out(adj.exprF(accumulator.rd))
      )
  }
}
