package shine.DPIA.IntermediatePrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.DSL._
import shine.DPIA.FunctionalPrimitives.{Cycle, Drop, Take}
import shine.DPIA.ImperativePrimitives.{CycleAcc, DropAcc, ForNat, TakeAcc}
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.language.reflectiveCalls

object SlideSeqIIndices {
  def apply(n: Nat,
            size: Nat,
            step: Nat,
            dt1: DataType,
            dt2: DataType,
            f: Phrase[ExpType ->: AccType ->: CommType],
            input: Phrase[ExpType],
            output: Phrase[AccType])
           (implicit context: TranslationContext): Phrase[CommType] =
  {
    assert(step.eval == 1) // FIXME?
    val inputSize = step * n + size - step

    `new`(size`.`dt1, buffer => {
      MapSeqI(size - 1, dt1, dt1, fun(expT(dt1, read))(exp => fun(accT(dt1))(acc => acc :=| dt1 | exp)),
        Take(size - 1, inputSize - size + 1, read, dt1, input),
        TakeAcc(size - 1, size - size + 1, dt1, buffer.wr)) `;`
        ForNat(n, _Λ_[NatKind]()(i => {
          ((DropAcc(size - 1, n, dt1,
            CycleAcc(size - 1 + n, size, dt1, buffer.wr)) `@` i) :=| dt1 |
            (Drop(size - 1, inputSize - size + 1, read, dt1, input) `@` i)) `;`
            f(Take(3, n - i - 3, read, dt1, Drop(i, n - i, read, dt1, Cycle(n, size, dt1, buffer.rd))))(output `@` i)
        }), unroll = false)
    })
  }
}