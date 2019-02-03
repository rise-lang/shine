package idealised.DPIA.IntermediatePrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA._
import idealised.DPIA.DSL._
import idealised.DPIA.FunctionalPrimitives.Take
import idealised.DPIA.ImperativePrimitives.TakeAcc
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
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
    val `n^k*m` = n.pow(k) * m

    newDoubleBuffer(dt"[${`n^k*m`}.$dt]", dt"[$m.$dt]", ArrayType(`n^k*m`, dt), in, out,
      (v: Phrase[VarType],
       swap: Phrase[CommandType],
       done: Phrase[CommandType]) => {
        `for`(k, ip => {
          val i = NamedVar(ip.name)

          f.apply(n.pow(k - i) * m)
            .apply(TakeAcc(n.pow(k - i - 1) * m, `n^k*m`, dt, v.wr))
            .apply(Take(n.pow(k - i) * m, `n^k*m`, dt, v.rd)) `;`
            IfThenElse(ip < Literal(IndexData(k - 1, IndexType(k))), swap, done)
        })
      })
  }
}
