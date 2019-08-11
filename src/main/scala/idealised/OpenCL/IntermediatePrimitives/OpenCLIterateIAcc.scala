package idealised.OpenCL.IntermediatePrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.DSL.{newDoubleBuffer => _, _}
import idealised.DPIA.FunctionalPrimitives._
import idealised.DPIA.ImperativePrimitives.TakeAcc
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.DSL.newDoubleBuffer
import lift.arithmetic.{Cst, NamedVar, RangeAdd}

object OpenCLIterateIAcc {
  def apply(a: AddressSpace,
            n: Nat,
            m: Nat,
            k: Nat,
            dt: DataType,
            out: Phrase[AccType],
            f: Phrase[`(nat)->:`[AccType ->: ExpType ->: CommType]],
            in: Phrase[ExpType])
           (implicit context: TranslationContext): Phrase[CommType] =
  {
    val sz = n.pow(k) * m

    newDoubleBuffer(a, dt"[$sz.$dt]", dt"[$m.$dt]", ArrayType(sz, dt), in, out,
      (v, swap, done) => {
        `for`(k, ip => {
          val i = NamedVar(ip.name, RangeAdd(Cst(0), k, Cst(1)))

          val isz = n.pow(k - i) * m
          val osz = n.pow(k - i - 1) * m
          f.apply(osz)
            .apply(TakeAcc(osz, sz - osz, dt, v.wr))
            .apply(Take(isz, sz - isz, read, dt, v.rd)) `;`
            IfThenElse(ip < AsIndex(k, Natural(k - 2)), swap, done)
        })
      })
  }
}
