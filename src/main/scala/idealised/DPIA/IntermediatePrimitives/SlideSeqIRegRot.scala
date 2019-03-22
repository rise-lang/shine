package idealised.DPIA.IntermediatePrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA._
import idealised.DPIA.DSL._
import idealised.DPIA.FunctionalPrimitives.{Drop, Take}
import idealised.DPIA.ImperativePrimitives._
import idealised.DPIA.Types._
import idealised.DPIA.Phrases._

import scala.language.reflectiveCalls

object SlideSeqIRegRot {
  def apply(n: Nat,
            size: Nat,
            // step: Nat,
            dt1: DataType,
            dt2: DataType,
            f: Phrase[ExpType -> (AccType -> CommandType)],
            input: Phrase[ExpType],
            output: Phrase[AccType])
           (implicit context: TranslationContext): Phrase[CommandType] =
  {
    val step = 1
    val inputSize = step * n + size - step

    NewRegRot(size, dt1,
      fun(exp"[$size.$dt1]" x acc"[$size.$dt1]")(rs =>
        fun(comm : CommandType)(rotate => {
          // prologue initialisation
          MapSeqI(size - 1, dt1, dt1, fun(ExpType(dt1))(exp => fun(AccType(dt1))(acc => acc :=|dt1| exp)),
            Take(size - 1, inputSize, dt1, input),
            TakeAcc(size - 1, size, dt1, rs.wr)) `;`
          // core loop
          ForNat(n, _Λ_(i => {
            // load current value
            ((rs.wr `@` (size - 1)) :=|dt1| (Drop(size - 1, inputSize, dt1, input) `@` i)) `;`
            f(rs.rd)(output `@` i) `;` // body
            rotate
          }), unroll = false)
        })))
  }
}