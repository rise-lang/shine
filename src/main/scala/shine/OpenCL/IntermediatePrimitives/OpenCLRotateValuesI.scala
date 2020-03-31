package shine.OpenCL.IntermediatePrimitives

import shine.DPIA.DSL._
import shine.DPIA.FunctionalPrimitives.{Drop}
import shine.DPIA.ImperativePrimitives._
import shine.DPIA.IntermediatePrimitives.MapSeqI
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

object OpenCLRotateValuesI {
  def apply(
    a: AddressSpace,
    n: Nat,
    size: Nat,
    dt: DataType,
    write_dt: Phrase[ExpType ->: AccType ->: CommType],
    nextInput: Phrase[`(nat)->:`[(ExpType ->: CommType) ->: CommType]],
    nextC: Phrase[`(nat)->:`[(ExpType ->: CommType) ->: CommType] ->: CommType]
  ): Phrase[CommType] = {
    // TODO: unroll flags?
    shine.OpenCL.DSL.`new`(a)(size`.`dt, rs => {
      // prologue initialisation
      forNat(size - 1, i => streamNext(nextInput, i, fun(expT(dt, read))(x =>
        write_dt(x)(rs.wr `@` i)
      )), unroll = true) `;`
      nextC(nFun(i =>
        fun(expT(size`.`dt, read) ->: (comm: CommType))(k =>
          // load next value
          streamNext(nextInput, i + size - 1, fun(expT(dt, read))(x =>
            write_dt(x)(rs.wr `@` (size - 1))
          )) `;`
          // use neighborhood
          k(rs.rd) `;`
          // rotate
          MapSeqI(size - 1, dt, dt, write_dt,
            Drop(1, size - 1, read, dt, rs.rd),
            TakeAcc(size - 1, 1, dt, rs.wr), unroll = true)
        ),
      arithexpr.arithmetic.RangeAdd(0, n, 1)))
    })
  }
}