package shine.OpenCL.primitives.intermediate

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.acc
import shine.DPIA.DSL.{`new` => _, _}
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.AdjustArraySizesForAllocations
import shine.OpenCL.DSL._

object ReduceSeqI {
  def apply(n: Nat,
            initAddrSpace: shine.DPIA.Types.AddressSpace,
            dt1: DataType, dt2: DataType,
            f: Phrase[ExpType ->: ExpType ->: AccType ->: CommType],
            init: Phrase[ExpType],
            in: Phrase[ExpType],
            out: Phrase[ExpType ->: CommType],
            unroll: Boolean)
           (implicit context: TranslationContext): Phrase[CommType] = {
    val adj = AdjustArraySizesForAllocations(init, dt2, initAddrSpace)

    comment("oclReduceSeq") `;`
      (`new` (initAddrSpace) (adj.dt, accumulator =>
        acc(init)(adj.accF(accumulator.wr)) `;`
          `for`(n, i => f(adj.exprF(accumulator.rd))(in `@` i)(adj.accF(accumulator.wr)), unroll) `;`
          out(adj.exprF(accumulator.rd))
      ))
  }
}
