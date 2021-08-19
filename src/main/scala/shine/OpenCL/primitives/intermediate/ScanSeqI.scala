package shine.OpenCL.primitives.intermediate


import rise.core.types.{AddressSpace, DataType}
import rise.core.types.DataType._
import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.acc
import shine.DPIA.DSL.{`new` => _, _}
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.functional.{Cast, IndexAsNat, NatAsIndex}
import shine.OpenCL.DSL._



object ScanSeqI {
  def apply(unroll:Boolean)(n: Nat, a: AddressSpace, dt1: DataType, dt2: DataType,
            f: Phrase[ExpType ->: ExpType ->: AccType ->: CommType],
            init: Phrase[ExpType],
            in: Phrase[ExpType],
            out: Phrase[AccType])
           (implicit context: TranslationContext): Phrase[CommType] =
  {
    comment("oclScanSeq")`;`
      `new`(a)(dt2, accumulator =>
        acc(init)(accumulator.wr) `;`
          `for`(n-1, idx => {
            val i = Cast(IndexType(n-1), IndexType(n), idx)
            ((out `@` i) :=| dt2 | accumulator.rd) `;`
              f(in `@` i)(accumulator.rd)(accumulator.wr)
          }, unroll) `;`
          ((out `@` NatAsIndex(n, Natural(n-1))) :=|dt2| accumulator.rd)
      )
  }
}