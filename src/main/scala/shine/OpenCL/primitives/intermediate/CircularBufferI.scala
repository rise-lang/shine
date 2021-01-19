package shine.OpenCL.primitives.intermediate

import arithexpr.arithmetic.ArithExpr
import shine.DPIA.DSL._
import shine.DPIA.primitives.imperative.{CycleAcc, DropAcc, PairAcc, UnzipAcc}
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._
import shine.DPIA.primitives.functional.{Cycle, Drop, Zip}

object CircularBufferI {
  def apply(
    a: AddressSpace,
    n: Nat,
    alloc: Nat,
    size: Nat,
    dt1: DataType,
    dt2: DataType,
    load: Phrase[ExpType ->: AccType ->: CommType],
    nextInput: Phrase[`(nat)->:`[(ExpType ->: CommType) ->: CommType]],
    nextC: Phrase[`(nat)->:`[(ExpType ->: CommType) ->: CommType] ->: CommType]
  ): Phrase[CommType] = {
    if (!ArithExpr.isSmaller(size, alloc + 1).contains(true)) {
      println(s"WARNING: circular buffer of size $alloc used for"
        + s" a sliding window of size $size")
    }

    // TODO: unroll flags?
    def gen(bufWr: Phrase[AccType], bufRd: Phrase[ExpType]): Phrase[CommType] = {
      // prologue initialisation
      forNat(size - 1, i => streamNext(nextInput, i, fun(expT(dt1, read))(x =>
        load(x)(bufWr `@` i)
      ))) `;`
      nextC(nFun(i =>
        fun(expT(size`.`dt2, read) ->: (comm: CommType))(k =>
          // load next value
          streamNext(nextInput, i + size - 1, fun(expT(dt1, read))(x =>
            load(x)(DropAcc(size - 1, n, dt2,
              CycleAcc(size - 1 + n, alloc, dt2, bufWr)) `@` i)
          )) `;`
          // use neighborhood
          k(Drop(i, size, dt2,
            Cycle(i + size, alloc, dt2, bufRd)))
        ),
        arithexpr.arithmetic.RangeAdd(0, n, 1)
      ))
    }

    // TODO: generalize and make explicit? might not always be wanted
    def allocGen(
      dt: DataType,
      C: (Phrase[AccType], Phrase[ExpType]) => Phrase[CommType]
    ): Phrase[CommType] = dt match {
      case PairType(a, b) =>
        allocGen(a, (wa, ra) => allocGen(b, (wb, rb) =>
          C(UnzipAcc(alloc, a, b, PairAcc(alloc`.`a, alloc`.`b, wa, wb)),
            Zip(alloc, a, b, read, ra, rb))
        ))
      case _ =>
        shine.OpenCL.DSL.`new`(a)(alloc`.`dt, buffer => C(buffer.wr, buffer.rd))
    }

    allocGen(dt2, gen)
  }
}