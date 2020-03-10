package shine.DPIA.IntermediatePrimitives

import shine.DPIA.DSL._
import shine.DPIA.FunctionalPrimitives.{Cycle, Drop, Take}
import shine.DPIA.ImperativePrimitives.{CycleAcc, DropAcc}
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

object SlideSeqIIndices {
  def apply(
    n: Nat,
    size: Nat,
    step: Nat,
    dt: DataType,
    write_dt: Phrase[ExpType ->: AccType ->: CommType],
    nextInput: Phrase[`(nat)->:`[(ExpType ->: CommType) ->: CommType]],
    nextC: Phrase[`(nat)->:`[(ExpType ->: CommType) ->: CommType] ->: CommType]
  ): Phrase[CommType] = {
    assert(step.eval == 1) // FIXME?

    // TODO: unroll flags?
    `new`(size`.`dt, buffer => {
      // prologue initialisation
      forNat(size - 1, i => streamNext(nextInput, i, fun(expT(dt, read))(x =>
        write_dt(x)(buffer.wr `@` i)
      ))) `;`
      nextC(nFun(i =>
        fun(expT(size`.`dt, read) ->: (comm: CommType))(k =>
          // load next value
          streamNext(nextInput, i + size - 1, fun(expT(dt, read))(x =>
            write_dt(x)(DropAcc(size - 1, n, dt,
              CycleAcc(size - 1 + n, size, dt, buffer.wr)) `@` i)
          )) `;`
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