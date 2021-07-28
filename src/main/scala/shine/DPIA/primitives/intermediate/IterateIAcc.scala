package shine.DPIA.primitives.intermediate

import arithexpr.arithmetic.NamedVar
import rise.core.types.DataType
import rise.core.types.DataType._
import shine.DPIA.DSL._
import shine.DPIA.primitives.imperative.TakeAcc
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.functional.{NatAsIndex, Take}

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
            .apply(Take(isz, sz - isz, dt, v.rd)) `;`
            IfThenElse(ip < NatAsIndex(k, Natural(k - 2)), swap, done)
        })
      })
  }
}
