package shine.OpenCL.IntermediatePrimitives

import shine.DPIA.DSL._
import shine.DPIA.FunctionalPrimitives.{Cycle, Drop, Take}
import shine.DPIA.ImperativePrimitives.{CycleAcc, DropAcc, TakeAcc}
import shine.DPIA.IntermediatePrimitives.MapSeqI
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

object OpenCLSlideSeqIIndices {
  def apply(
    a: AddressSpace,
    n: Nat,
    size: Nat,
    step: Nat,
    dt: DataType,
    write_dt: Phrase[ExpType ->: AccType ->: CommType],
    input: Phrase[ExpType],
    nextC: Phrase[`(nat)->:`[(ExpType ->: CommType) ->: CommType] ->: CommType]
  ): Phrase[CommType] = {
    assert(step.eval == 1) // FIXME?
    val inputSize = step * n + size - step

    // TODO: unroll flags?
    shine.OpenCL.DSL.`new`(a)(size`.`dt, buffer => {
      // prologue initialisation
      MapSeqI(size - 1, dt, dt, write_dt,
        Take(size - 1, inputSize - size + 1, read, dt, input),
        TakeAcc(size - 1, size - size + 1, dt, buffer.wr)) `;`
      nextC(nFun(i =>
        fun(expT(size`.`dt, read) ->: (comm: CommType))(k =>
          // load next value
          write_dt(
            Drop(size - 1, inputSize - size + 1, read, dt, input) `@` i,
          )(DropAcc(size - 1, n, dt,
            CycleAcc(size - 1 + n, size, dt, buffer.wr)) `@` i
          ) `;`
          // use neighborhood
          k(Take(size, n - i - size, read, dt,
            Drop(i, n - i, read, dt, Cycle(n, size, dt, buffer.rd)))
          )
        ),
        arithexpr.arithmetic.RangeAdd(0, n, 1)
      ))
    })
  }
}