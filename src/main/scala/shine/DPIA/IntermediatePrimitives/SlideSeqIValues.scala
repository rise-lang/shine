package shine.DPIA.IntermediatePrimitives

import shine.DPIA.DSL._
import shine.DPIA.FunctionalPrimitives.{Drop, Take}
import shine.DPIA.ImperativePrimitives._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

object SlideSeqIValues {
  def apply(
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
    `new`(size`.`dt, fun(varT(size`.`dt))(rs => {
      // prologue initialisation
      MapSeqI(size - 1, dt, dt, write_dt,
        Take(size - 1, inputSize - size + 1, read, dt, input),
        TakeAcc(size - 1, size - size + 1, dt, rs.wr), unroll = true) `;`
      nextC(nFun(i =>
        fun(expT(size`.`dt, read) ->: (comm: CommType))(k =>
          // load next value
          write_dt(
            Drop(size - 1, inputSize - size + 1, read, dt, input) `@` i
          )(rs.wr `@` (size - 1)) `;`
          // use neighborhood
          k(rs.rd) `;`
          // rotate
          MapSeqI(size - 1, dt, dt, write_dt,
            Drop(1, size - 1, read, dt, rs.rd),
            TakeAcc(size - 1, 1, dt, rs.wr), unroll = true)
        ),
      arithexpr.arithmetic.RangeAdd(0, n, 1)))
    }))
  }
}