package idealised.DPIA.IntermediatePrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.language.reflectiveCalls

object ReduceSeqI {
  def apply(n: Nat, dt1: DataType, dt2: DataType,
            f: Phrase[ExpType -> (ExpType -> (AccType -> CommandType))],
            init: Phrase[ExpType],
            in: Phrase[ExpType],
            out: Phrase[ExpType -> CommandType])
           (context: TranslationContext): Phrase[CommandType] =
  {
    implicit val c = context
      
    // TODO: generalise allocation
    `new`(dt2, idealised.OpenCL.PrivateMemory, acc =>
      (acc.wr :=|dt2| init) `;`
        `for`(n, i => f(in `@` i)(acc.rd)(acc.wr)) `;`
        out(acc.rd)
    )
  }
}
