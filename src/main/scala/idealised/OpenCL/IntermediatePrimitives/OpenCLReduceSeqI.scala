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
            out: Phrase[ExpType ->: CommType])
           (implicit context: TranslationContext): Phrase[CommType] = {
    val (adjAcc, adjExpr, adjDt) = AdjustArraySizesForAllocations(init, dt2, initAddrSpace)
    println(s"OldDt: $dt2")
    println(s"Adjusted: $adjDt")

    comment("oclReduceSeq") `;`
      `new` (initAddrSpace) (adjDt, accumulator =>
        acc(init)(adjAcc(accumulator.wr)) `;`
          `for`(n, i => f(adjExpr(accumulator.rd))(in `@` i)(adjAcc(accumulator.wr))) `;`
          out(adjExpr(accumulator.rd))
      )
  }
}
