package idealised.DPIA.IntermediatePrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.DSL._
import idealised.DPIA.FunctionalPrimitives.{AsIndex, Take}
import idealised.DPIA.ImperativePrimitives.TakeAcc
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._
import lift.arithmetic.NamedVar

object IterateIAcc {
  def apply(n: Nat,
            m: Nat,
            k: Nat,
            dt: DataType,
            out: Phrase[AccType],
            f: Phrase[`(nat)->`[AccType -> (ExpType -> CommandType)]],
            in: Phrase[ExpType])
           (implicit context: TranslationContext): Phrase[CommandType] =
  {
    val sz = n.pow(k) * m

    newDoubleBuffer(dt"[$sz.$dt]", dt"[$m.$dt]", ArrayType(sz, dt), in, out,
      (v: Phrase[VarType],
       swap: Phrase[CommandType],
       done: Phrase[CommandType]) => {
        `for`(k, ip => {
          val i = NamedVar(ip.name)

          val isz = n.pow(k - i) * m
          val osz = n.pow(k - i - 1) * m
          f.apply(osz)
            .apply(TakeAcc(osz, sz - osz, dt, v.wr))
            .apply(Take(isz, sz - isz, read, dt, v.rd)) `;`
            IfThenElse(ip < AsIndex(k, Natural(k - 1)), swap, done)
        })
      })
  }
}
