package idealised.OpenCL.IntermediatePrimitives

import idealised.DPIA.DSL.{`new` => _, _}
import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.AddressSpace
import idealised.OpenCL.DSL._

import scala.language.reflectiveCalls

object OpenCLReduceSeqI {
  def apply(n: Nat, dt1: DataType, dt2: DataType,
            f: Phrase[ExpType ->: ExpType ->: AccType ->: CommType],
            init: Phrase[ExpType],
            initAddrSpace: AddressSpace,
            in: Phrase[ExpType],
            out: Phrase[ExpType ->: CommType],
            unroll: Boolean)
           (implicit context: TranslationContext): Phrase[CommType] =
  {
    newWithAddrSpace(dt2, initAddrSpace, acc =>
      (acc.wr :=|dt2| init) `;`
        `for`(n, i => f(in `@` i)(acc.rd)(acc.wr), unroll) `;`
        out(acc.rd)
    )
  }
}
