package shine.DPIA.Compilation

import rise.core.types.{DataType, NatIdentifier, NatKind, read}
import rise.core.types.DataType._
import rise.core.DSL.Type._
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.functional._
import shine.DPIA.primitives.imperative.{CycleAcc, DropAcc, PairAcc, TakeAcc, UnzipAcc, Seq => _}
import shine.OpenCL.primitives.{functional => ocl}

import scala.annotation.tailrec

object StreamTranslation {
  @tailrec
  def str(E: Phrase[ExpType])
         (C: Phrase[`(nat)->:`[(ExpType ->: CommType) ->: CommType] ->: CommType])
         (implicit context: TranslationContext): Phrase[CommType] = {
    E match {
      case ep: ExpPrimitive => primitive(ep)(C)

      // on the fly beta-reduction
      case Apply(fun, arg) => str(Lifting.liftFunction(fun).reducing(arg))(C)
      case DepApply(_, fun, arg) => arg match {
        case a: Nat => str(
          Lifting.liftDependentFunction(
            fun.asInstanceOf[Phrase[`(nat)->:`[ExpType]]])(a)
        )(C)
        case a: DataType => str(
          Lifting.liftDependentFunction(
            fun.asInstanceOf[Phrase[`(dt)->:`[ExpType]]])(a)
        )(C)
      }

      case _ => translateArrayToStream(E, C)
    }
  }

  def primitive(E: ExpPrimitive)
               (C: Phrase[`(nat)->:`[(ExpType ->: CommType) ->: CommType] ->: CommType])
               (implicit context: TranslationContext): Phrase[CommType] = E match {
    case CircularBuffer(n, _, size, dt1, dt2, load, input) =>
      val i = NatIdentifier(freshName("i"))
      str(input)(fun((i: NatIdentifier) ->:
        (expT(dt1, read) ->: (comm: CommType)) ->: (comm: CommType)
      )(nextInput => {
        // TODO: generalize and make explicit? might not always be wanted
        def allocGen(dt: DataType,
                     C: (Phrase[AccType], Phrase[ExpType]) => Phrase[CommType]): Phrase[CommType] = dt match {
          case PairType(a, b) =>
            allocGen(a, (wa, ra) => allocGen(b, (wb, rb) =>
              C(UnzipAcc(size, a, b, PairAcc(size `.` a, size `.` b, wa, wb)),
                Zip(size, a, b, `read`, ra, rb))
            ))
          case _ =>
            `new`(size `.` dt, buffer => C(buffer.wr, buffer.rd))
        }

        allocGen(dt2, (bufWr: Phrase[AccType], bufRd: Phrase[ExpType]) => {
          // TODO: unroll flags?
          // prologue initialisation
          forNat(n = size - 1, f = i =>
            streamNext(nextInput, i, fun(expT(dt1, read))(x => acc(load(x))(bufWr `@` i)))) `;`
          C(nFun(i =>
            fun(expT(size `.` dt2, read) ->: (comm: CommType))(k =>
              // load next value
              streamNext(nextInput, i + size - 1, fun(expT(dt1, read))(x =>
                acc(load(x))(DropAcc(size - 1, n, dt2,
                  CycleAcc(size - 1 + n, size, dt2, bufWr)) `@` i)
              )) `;`
                // use neighborhood
                k(Take(size, n - i - size, dt2,
                  Drop(i, n - i, dt2, Cycle(n, size, dt2, bufRd))))
            ),
            arithexpr.arithmetic.RangeAdd(0, n, 1)
          ))
        })
      }))

    case MapStream(n, dt1, dt2, f, array) =>
      val i = NatIdentifier(freshName("i"))
      str(array)(fun((i: NatIdentifier) ->:
        (expT(dt1, read) ->: (comm: CommType)) ->: (comm: CommType)
      )(next =>
        C(nFun(i =>
          fun(expT(dt2, read) ->: (comm: CommType))(k =>
            streamNext(next, i, fun(expT(dt1, read))(x =>
              con(f(x))(k)
            ))
          ), arithexpr.arithmetic.RangeAdd(0, n, 1)))))

    case RotateValues(n, size, dt, write, input) =>
      val i = NatIdentifier(freshName("i"))
      str(input)(fun((i: NatIdentifier) ->:
        (expT(dt, read) ->: (comm: CommType)) ->: (comm: CommType)
      )(nextInput => {
        // TODO: unroll flags?
        `new`(size `.` dt, fun(varT(size `.` dt))(rs => {
          // prologue initialisation
          forNat(unroll = true, size - 1, i =>
            streamNext(nextInput, i, fun(expT(dt, read))(x => acc(write(x))(rs.wr `@` i) ))) `;`
          C(nFun(i =>
            fun(expT(size `.` dt, read) ->: (comm: CommType))(k =>
              // load next value
              streamNext(nextInput, i + size - 1, fun(expT(dt, read))(x =>
                acc(write(x))(rs.wr `@` (size - 1))
              )) `;`
                // use neighborhood
                k(rs.rd) `;`
                // rotate
                comment("mapSeq") `;`
                `for`(unroll = true, size - 1, i =>
                  acc(write(Drop(1, size - 1, dt, rs.rd) `@` i))(TakeAcc(size - 1, 1, dt, rs.wr) `@` i))
            ),
            arithexpr.arithmetic.RangeAdd(0, n, 1)))
        }))
      }))

    case Zip(n, dt1, dt2, _, e1, e2) =>
      val i = NatIdentifier("i")
      str(e1)(fun((i: NatIdentifier) ->:
        (expT(dt1, read) ->: (comm: CommType)) ->: (comm: CommType)
      )(next1 =>
        str(e2)(fun((i: NatIdentifier) ->:
          (expT(dt2, read) ->: (comm: CommType)) ->: (comm: CommType)
        )(next2 =>
          C(nFun(i => fun(expT(dt1 x dt2, read) ->: (comm: CommType))(k =>
            Apply(DepApply(NatKind, next1, i),
              fun(expT(dt1, read))(x1 =>
                Apply(DepApply(NatKind, next2, i),
                  fun(expT(dt2, read))(x2 =>
                    k(MakePair(dt1, dt2, read, x1, x2))
                  ))))),
            arithexpr.arithmetic.RangeAdd(0, n, 1)))))))

    // OpenCL
    case ocl.CircularBuffer(a, n, alloc, size, dt1, dt2, load, input) =>
      val i = NatIdentifier(freshName("i"))
      str(input)(fun((i: NatIdentifier) ->:
        (expT(dt1, read) ->: (comm: CommType)) ->: (comm: CommType)
      )(nextInput => {
        if (!arithexpr.arithmetic.ArithExpr.isSmaller(size, alloc + 1).contains(true)) {
          println(s"WARNING: circular buffer of size $alloc used for"
            + s" a sliding window of size $size")
        }

        // TODO: generalize and make explicit? might not always be wanted
        def allocGen(dt: DataType,
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

        allocGen(dt2, (bufWr: Phrase[AccType], bufRd: Phrase[ExpType]) => {
          // TODO: unroll flags?
          // prologue initialisation
          forNat(n = size - 1, f = i => streamNext(nextInput, i, fun(expT(dt1, read))(x =>
            acc(load(x))(bufWr `@` i)))) `;`
            C(nFun(i =>
              fun(expT(size`.`dt2, read) ->: (comm: CommType))(k =>
                // load next value
                streamNext(nextInput, i + size - 1, fun(expT(dt1, read))(x =>
                  acc(load(x))(DropAcc(size - 1, n, dt2,
                    CycleAcc(size - 1 + n, alloc, dt2, bufWr)) `@` i)
                )) `;`
                  // use neighborhood
                  k(Drop(i, size, dt2,
                    Cycle(i + size, alloc, dt2, bufRd)))
              ),
              arithexpr.arithmetic.RangeAdd(0, n, 1)
            ))
        })
      }
//        oclI.CircularBufferI(a, n, alloc, sz, dt1, dt2,
//          fun(expT(dt1, read))(x =>
//            fun(accT(dt2))(o => acc(load(x))(o))),
//          nextIn, C)
      ))

    case ocl.RotateValues(a, n, size, dt, write, input) =>
      val i = NatIdentifier(freshName("i"))
      str(input)(fun((i: NatIdentifier) ->:
        (expT(dt, read) ->: (comm: CommType)) ->: (comm: CommType)
      )(nextInput => {
        // TODO: unroll flags?
        shine.OpenCL.DSL.`new`(a)(size`.`dt, rs => {
          // prologue initialisation
          forNat(unroll = true, size - 1, i => streamNext(nextInput, i, fun(expT(dt, read))(x =>
            acc(write(x))(rs.wr `@` i) ))) `;`
          C(nFun(i =>
            fun(expT(size`.`dt, read) ->: (comm: CommType))(k =>
              // load next value
              streamNext(nextInput, i + size - 1, fun(expT(dt, read))(x =>
                acc(write(x))(rs.wr `@` (size - 1))
              )) `;`
              // use neighborhood
              k(rs.rd) `;`
              // rotate
              comment("mapSeq")`;`
              `for`(unroll = true, size - 1, i =>
                acc(write(Drop(1, size - 1, dt, rs.rd) `@` i))(TakeAcc(size - 1, 1, dt, rs.wr) `@` i))
            ),
            arithexpr.arithmetic.RangeAdd(0, n, 1)))
        })
      }))

    case _ => translateArrayToStream(E, C)
  }

  // TODO: works for arrays, but not streams
  def translateArrayToStream(E: Phrase[ExpType],
                              C: Phrase[`(nat)->:`[(ExpType ->: CommType) ->: CommType] ->: CommType])(
                              implicit context: TranslationContext
                            ): Phrase[CommType] = {
    import shine.DPIA.DSL._

    E.t match {
      case ExpType(ArrayType(n, dt), read) =>
        C(nFun(i => fun(expT(dt, read) ->: (comm: CommType))(k =>
          con(E `@` i)(k)
        ), arithexpr.arithmetic.RangeAdd(0, n, 1)))
      case _ => throw new Exception("this should not happen")
    }
  }
}
