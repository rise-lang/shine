package shine.DPIA.DSL

import rise.core.types.{DataType, NatKind, read}
import rise.core.types.DataType._
import shine.DPIA.primitives.imperative._
import shine.DPIA.Phrases.{Identifier, IfThenElse, Phrase}
import shine.DPIA.Types._
import rise.core.DSL.Type._
import shine.DPIA._
import shine.DPIA.primitives.functional.{Fst, Snd}

object ImperativePrimitives {
  def skip: Skip = Skip()
}

object `new` {
  def apply(dt: DataType,
            f: Phrase[VarType ->: CommType]): New =
    New(dt, f)

  def apply(dt: DataType,
            f: Phrase[VarType] => Phrase[CommType]): New =
    New(dt, fun(varT(dt))( v => f(v) ))
}

object newDoubleBuffer {
  def apply(dt1: DataType,
            dt2: DataType,
            dt3: ArrayType,
            in: Phrase[ExpType],
            out: Phrase[AccType],
            f: (Phrase[VarType], Phrase[CommType], Phrase[CommType]) => Phrase[CommType]): NewDoubleBuffer =
    NewDoubleBuffer(dt1, dt2, dt3.elemType, dt3.size, in, out, fun(varT(dt1) x CommType() x CommType())(ps => {
      val    v: Phrase[VarType]  = ps.`1`.`1`
      val swap: Phrase[CommType] = ps.`1`.`2`
      val done: Phrase[CommType] = ps.`2`
      f(v, swap, done)
    }))
}

object `if` {
  def apply[T <: PhraseType](cond: Phrase[ExpType],
                             thenP: Phrase[T],
                             elseP: Phrase[T]): IfThenElse[T] =
    IfThenElse(cond, thenP, elseP)

  //noinspection TypeAnnotation
  def apply(cond: Phrase[ExpType]): IfHelper = IfHelper(cond)

  case class IfHelper(cond: Phrase[ExpType]) {
    def `then`[T <: PhraseType](thenP: Phrase[T]): IfHelper.ThenHelper[T] = IfHelper.ThenHelper(cond, thenP)
  }
  object IfHelper {
    case class ThenHelper[T <: PhraseType](cond: Phrase[ExpType], thenP: Phrase[T]) {
      def `else`(elseP: Phrase[T]): IfThenElse[T] = {
        IfThenElse(cond, thenP, elseP)
      }
    }
  }
}

object `for` {
  def apply(n: Nat, f: Identifier[ExpType] => Phrase[CommType]): For = apply(false, n, f)
  def apply(unroll: Boolean, n: Nat, f: Identifier[ExpType] => Phrase[CommType]): For =
    For(unroll)(n, fun(expT(idx(n), read))( i => f(i) ))
}

object forNat {
  def apply(n: Nat, f: NatIdentifier => Phrase[CommType]): ForNat = apply(false, n, f)
  def apply(unroll: Boolean, n: Nat, f: NatIdentifier => Phrase[CommType]): ForNat = {
    import arithexpr.arithmetic.RangeAdd
    ForNat(unroll)(n, nFun(i => f(i), RangeAdd(0, n, 1)))
  }
}

object streamNext {
  def apply(
    next: Phrase[`(nat)->:`[(ExpType ->: CommType) ->: CommType]],
    i: Nat,
    f: Phrase[ExpType ->: CommType]
  ): Phrase[CommType] = {
    Phrases.Apply(
      Phrases.DepApply(NatKind, next, i),
      f
    )
  }
}

object comment {
  def apply(comment: String): Comment = Comment(comment)()
}

object fst {
  def apply(pair: Phrase[ExpType]): Fst = {
    pair.t match {
      case ExpType(PairType(dt1, dt2), _) => Fst(dt1, dt2, pair)
      case x => error(x.toString, "ExpType(RecordType)")
    }
  }
}

object snd {
  def apply(pair: Phrase[ExpType]): Snd = {
    pair.t match {
      case ExpType(PairType(dt1, dt2), _) => Snd(dt1, dt2, pair)
      case x => error(x.toString, "ExpType(RecordType)")
    }
  }
}

object pairAcc1 {
  def apply(fstT: DataType, sndT: DataType, record: Phrase[AccType]): PairAcc1 =
    PairAcc1(fstT, sndT, record)
}

object pairAcc2 {
  def apply(fstT: DataType, sndT: DataType, record: Phrase[AccType]): PairAcc2 =
    PairAcc2(fstT, sndT, record)
}
