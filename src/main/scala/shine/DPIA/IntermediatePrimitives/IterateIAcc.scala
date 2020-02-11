package shine.DPIA.IntermediatePrimitives

import arithexpr.arithmetic.NamedVar
import shine.DPIA.DSL._
import shine.DPIA.FunctionalPrimitives.{AsIndex, Take}
import shine.DPIA.ImperativePrimitives.TakeAcc
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

object IterateIAcc {
  def apply(n: Nat,
            m: Nat,
            k: Nat,
            dt: DataType,
            out: Phrase[AccType],
            f: Phrase[`(nat)->:`[AccType ->: ExpType ->: CommType]],
            in: Phrase[ExpType]): Phrase[CommType] =
  {
    val sz = n.pow(k) * m

    newDoubleBuffer(sz`.`dt, m`.`dt, sz`.`dt, in, out,
      (v: Phrase[VarType],
       swap: Phrase[CommType],
       done: Phrase[CommType]) => {
        `for`(k, ip => {
          val i = NamedVar(ip.name)

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
