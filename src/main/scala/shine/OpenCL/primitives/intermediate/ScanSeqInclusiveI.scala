package shine.OpenCL.primitives.intermediate

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.acc
import shine.DPIA.DSL.{`new` => _, _}
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.functional.{IndexAsNat, NatAsIndex}
import shine.OpenCL.DSL._


object ScanSeqInclusiveI {
  def apply(n: Nat, a: AddressSpace, dt1: DataType, dt2: DataType,
            f: Phrase[ExpType ->: ExpType ->: AccType ->: CommType],
            init: Phrase[ExpType],
            in: Phrase[ExpType],
            out: Phrase[AccType])
           (implicit context: TranslationContext): Phrase[CommType] =
  {
    comment("oclScanSeqInclusive")`;`
      `new`(a)(dt2, accumulator =>
        acc(init)(accumulator.wr) `;`
          ((out `@` NatAsIndex(n+1, Natural(0))) :=| dt2 | accumulator.rd) `;`
          `for`(n, i =>
            f(in `@` i)(accumulator.rd)(accumulator.wr) `;`
              //FIXME remove general assignment
              //TODO: Do it with Drop, rather than manual index frobbing
              ((out `@` NatAsIndex(n+1, IndexAsNat(n,i)+Natural(1))) :=| dt2 | accumulator.rd)
          )
      )
  }
}