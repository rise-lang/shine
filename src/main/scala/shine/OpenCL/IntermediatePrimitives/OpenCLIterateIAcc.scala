package shine.OpenCL.IntermediatePrimitives

import arithexpr.arithmetic.{Cst, NamedVar, RangeAdd}
import shine.DPIA.DSL.{newDoubleBuffer => _, _}
import shine.DPIA.FunctionalPrimitives._
import shine.DPIA.ImperativePrimitives.TakeAcc
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._
import shine.OpenCL.DSL.newDoubleBuffer

object OpenCLIterateIAcc {
  def apply(a: AddressSpace,
            n: Nat,
            m: Nat,
            k: Nat,
            dt: DataType,
            out: Phrase[AccType],
            f: Phrase[`(nat)->:`[AccType ->: ExpType ->: CommType]],
            in: Phrase[ExpType]): Phrase[CommType] =
  {
    val sz = n.pow(k) * m

    newDoubleBuffer(a, sz`.`dt, m`.`dt, sz`.`dt, in, out,
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
