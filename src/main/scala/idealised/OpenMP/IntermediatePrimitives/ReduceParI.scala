package idealised.OpenMP.IntermediatePrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.language.reflectiveCalls

object ReduceParI {
  def apply(n: Nat, dt1: DataType, dt2: DataType,
            f: Phrase[ExpType ->: ExpType ->: AccType ->: CommType],
            init: Phrase[ExpType],
            in: Phrase[ExpType],
            out: Phrase[ExpType ->: CommType])
           (implicit context: TranslationContext): Phrase[CommType] =
  {
//    `new`(dt2, OpenCL.PrivateMemory, acc =>
//      (acc.wr :=|dt2| init) `;`
//        `for`(n, i =>
//          SubstituteImplementations(f(in `@` i)(acc.rd)(acc.wr), env)
//        ) `;`
//        out(acc.rd)
//    )
    ???
  }
}
