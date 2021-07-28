package shine.DPIA.Compilation

import rise.core.types.{ArrayType, DataType, NatIdentifier, NatKind, read}
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.functional._
import shine.DPIA.primitives.imperative.{Seq => _}
import shine.DPIA.primitives.intermediate._
import shine.OpenCL.primitives.{functional => ocl, intermediate => oclI}

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
    case CircularBuffer(n, alloc, sz, dt1, dt2, load, input) =>
      val i = NatIdentifier(freshName("i"))
      str(input)(fun((i: NatIdentifier) ->:
        (expT(dt1, read) ->: (comm: CommType)) ->: (comm: CommType)
      )(nextIn =>
        CircularBufferI(n, sz, 1, dt1, dt2,
          fun(expT(dt1, read))(x =>
            fun(accT(dt2))(o => acc(load(x))(o))),
          nextIn, C)))

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

    case RotateValues(n, sz, dt, write, input) =>
      val i = NatIdentifier(freshName("i"))
      str(input)(fun((i: NatIdentifier) ->:
        (expT(dt, read) ->: (comm: CommType)) ->: (comm: CommType)
      )(nextIn =>
        RotateValuesI(n, sz, 1, dt, dt,
          fun(expT(dt, read))(x =>
            fun(accT(dt))(o => acc(write(x))(o))),
          nextIn, C)))

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
    case ocl.CircularBuffer(a, n, alloc, sz, dt1, dt2, load, input) =>
      val i = NatIdentifier(freshName("i"))
      str(input)(fun((i: NatIdentifier) ->:
        (expT(dt1, read) ->: (comm: CommType)) ->: (comm: CommType)
      )(nextIn =>
        oclI.CircularBufferI(a, n, alloc, sz, dt1, dt2,
          fun(expT(dt1, read))(x =>
            fun(accT(dt2))(o => acc(load(x))(o))),
          nextIn, C)))

    case ocl.RotateValues(a, n, sz, dt, write, input) =>
      val i = NatIdentifier(freshName("i"))
      str(input)(fun((i: NatIdentifier) ->:
        (expT(dt, read) ->: (comm: CommType)) ->: (comm: CommType)
      )(nextIn =>
        oclI.RotateValuesI(a, n, sz, dt,
          fun(expT(dt, read))(x =>
            fun(accT(dt))(o => acc(write(x))(o))),
          nextIn, C)))

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
