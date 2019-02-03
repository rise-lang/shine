package idealised.DPIA.IntermediatePrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA._
import idealised.DPIA.DSL._
import idealised.DPIA.Types._
import idealised.DPIA.Phrases._
import idealised.DPIA.FunctionalPrimitives.{Cycle, Drop, Take}
import idealised.DPIA.ImperativePrimitives.{CycleAcc, DropAcc, ForNat, TakeAcc}

import scala.language.reflectiveCalls

object MapSeqSlideICircular {
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

    `new`(ArrayType(size, dt1), DefaultAddressSpace, buffer => {
      MapSeqI(size - 1, dt1, dt1, fun(ExpType(dt1))(exp => fun(AccType(dt1))(acc => acc :=| dt1 | exp)),
        Take(size - 1, inputSize, dt1, input),
        TakeAcc(size - 1, size, dt1, buffer.wr))(context) `;`
        ForNat(n, _Λ_(i => {
          ((DropAcc(size - 1, size - 1 + n, dt1,
            CycleAcc(size - 1 + n, size, dt1, buffer.wr)) `@` i) :=| dt1 |
            (Drop(size - 1, inputSize, dt1, input) `@` i)) `;`
            f(Take(3, n - i, dt1, Drop(i, n, dt1, Cycle(n, size, dt1, buffer.rd))))(output `@` i)
        }))
    })
  }
}