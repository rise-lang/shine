package idealised.OpenCL.IntermediatePrimitives

import idealised.DPIA.DSL.{`new` => _, _}
import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.Compilation.TranslationToImperative.acc
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.AdjustArraySizesForAllocations
import idealised.OpenCL.DSL._

import scala.language.reflectiveCalls

object OpenCLReduceSeqI {
  def apply(n: Nat,
            initAddrSpace: idealised.DPIA.Types.AddressSpace,
            dt1: DataType, dt2: DataType,
            f: Phrase[ExpType ->: ExpType ->: AccType ->: CommType],
            init: Phrase[ExpType],
            in: Phrase[ExpType],
            out: Phrase[ExpType ->: CommType],
            unroll: Boolean)
           (implicit context: TranslationContext): Phrase[CommType] = {
    val adj = AdjustArraySizesForAllocations(init, dt2, initAddrSpace)

    comment("oclReduceSeq") `;`
      `new` (initAddrSpace) (adj.dt, accumulator =>
        acc(init)(adj.accF(accumulator.wr)) `;`
          `for`(n, i => f(adj.exprF(accumulator.rd))(in `@` i)(adj.accF(accumulator.wr)), unroll) `;`
          out(adj.exprF(accumulator.rd))
      )
  }
}
