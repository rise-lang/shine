package shine.DPIA.IntermediatePrimitives

import shine.DPIA.DSL._
import shine.DPIA.FunctionalPrimitives.{Drop, Take}
import shine.DPIA.ImperativePrimitives._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.language.reflectiveCalls

object SlideSeqIValues {
  def apply(n: Nat,
            size: Nat,
            step: Nat,
            dt1: DataType,
            dt2: DataType,
            write_dt1: Phrase[ExpType ->: AccType ->: CommType],
            f: Phrase[ExpType ->: AccType ->: CommType],
            input: Phrase[ExpType],
            output: Phrase[AccType]): Phrase[CommType] =
  {
    assert(step.eval == 1) // FIXME?
    val inputSize = step * n + size - step

    // TODO: unroll flags?
    `new`(size`.`dt1, fun(varT(size`.`dt1))(rs => {
      // prologue initialisation
      MapSeqI(size - 1, dt1, dt1, write_dt1,
        Take(size - 1, inputSize - size + 1, read, dt1, input),
        TakeAcc(size - 1, size - size + 1, dt1, rs.wr), unroll = true) `;`
      // core loop
      ForNat(n, _Λ_[NatKind]()(i => {
        // load current value
        write_dt1(Drop(size - 1, inputSize - size + 1, read, dt1, input) `@` i)(rs.wr `@` (size - 1)) `;`
        f(rs.rd)(output `@` i) `;` // body
        // rotate
        MapSeqI(size - 1, dt1, dt1, write_dt1,
          Drop(1, size - 1, read, dt1, rs.rd),
          TakeAcc(size - 1, 1, dt1, rs.wr), unroll = true)
      }), unroll = false)
    }))
  }
}