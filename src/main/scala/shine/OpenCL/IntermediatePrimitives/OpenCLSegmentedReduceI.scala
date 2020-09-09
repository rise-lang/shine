package shine.OpenCL.IntermediatePrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.acc
import shine.DPIA.DSL.{`new` => _, _}
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.AdjustArraySizesForAllocations
import shine.OpenCL.DSL._

object OpenCLSegmentedReduceI {
  def apply(n: Nat,
            k: Nat,
            initAddrSpace: shine.DPIA.Types.AddressSpace,
            dt: DataType,
            f: Phrase[ExpType ->: ExpType ->: AccType ->: CommType],
            init: Phrase[ExpType],
            in: Phrase[ExpType],
            out: Phrase[ExpType ->: CommType])
           (implicit context: TranslationContext): Phrase[CommType] = {
    val adj = AdjustArraySizesForAllocations(init, ArrayType(k, dt), initAddrSpace)

    comment("oclSegmentedReduce") `;`
      `new` (initAddrSpace) (adj.dt, accumulator =>
        acc(init)(adj.accF(accumulator.wr)) `;`

          out(adj.exprF(accumulator.rd))
      )
  }
}
