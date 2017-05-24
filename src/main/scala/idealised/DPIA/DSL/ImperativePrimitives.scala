package idealised.DPIA.DSL

import lift.arithmetic.{NamedVar, RangeAdd}
import idealised.utils._
import idealised.DPIA.Phrases.{IfThenElse, NatDependentLambda, Phrase}
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.DPIA.FunctionalPrimitives.{Fst, Snd}
import idealised.DPIA.ImperativePrimitives._

object `new` {
  def apply(dt: DataType,
            addressSpace: AddressSpace,
            f: Phrase[(ExpType x AccType) -> CommandType]): New =
    New(dt, addressSpace, f)

  def apply(dt: DataType,
            addressSpace: AddressSpace,
            f: Phrase[ExpType x AccType] => Phrase[CommandType]): New =
    New(dt, addressSpace, λ(exp"[$dt]" x acc"[$dt]")( v => f(v) ))
}

object `if` {
  def apply[T <: PhraseType](cond: Phrase[ExpType],
                             thenP: Phrase[T],
                             elseP: Phrase[T]): IfThenElse[T] =
    IfThenElse(cond, thenP, elseP)

  def apply(cond: Phrase[ExpType]) = new {
    def `then`[T <: PhraseType](thenP: Phrase[T]) = new {
      def `else`(elseP: Phrase[T]): IfThenElse[T] = {
        IfThenElse(cond, thenP, elseP)
      }
    }
  }
}

object `for` {
  def apply(n: Nat,
            f: (Phrase[ExpType] => Phrase[CommandType])): For =
    For(n, λ(exp"[idx($n)]")( i => f(i) ))
}

object `parFor` {
  def apply(n: Nat,
            dt: DataType,
            out: Phrase[AccType],
            f: (Phrase[ExpType] => Phrase[AccType] => Phrase[CommandType])): ParFor =
    ParFor(n, dt, out, λ(exp"[idx($n)]")( i => λ(acc"[$dt]")( o => f(i)(o) )))
}

object dblBufFor {
  def apply(n: Nat,
            m: Nat,
            k: Nat,
            dt: DataType,
            addressSpace: AddressSpace,
            buffer1: Phrase[VarType],
            buffer2: Phrase[VarType],
            body: Phrase[`(nat)->`[AccType -> (ExpType -> CommandType)]],
            C: Phrase[ExpType -> CommandType]): DoubleBufferFor = {
    body match {
      case NatDependentLambda(x, b) =>
        // ensure that the nested Nat dependent lambda has the proper range information
        val newX = NamedVar(x.name, RangeAdd(0, k, 1))
        val newB = PhraseType.substitute(newX, x, b)
        DoubleBufferFor(n, m, k, dt, addressSpace, buffer1, buffer2,
          NatDependentLambda(newX, newB),
          C)
      case _ => throw new Exception("This should not happen")
    }
  }
}

object fst {
  def apply(record: Phrase[ExpType]): Fst = {
    record.t match {
      case ExpType(RecordType(dt1, dt2)) => Fst(dt1, dt2, record)
      case x => error(x.toString, "ExpType(RecordType)")
    }
  }
}

object snd {
  def apply(record: Phrase[ExpType]): Snd = {
    record.t match {
      case ExpType(RecordType(dt1, dt2)) => Snd(dt1, dt2, record)
      case x => error(x.toString, "ExpType(RecordType)")
    }
  }
}

object recordAcc1 {
  def apply(fstT: DataType, sndT: DataType, record: Phrase[AccType]): RecordAcc1 =
    RecordAcc1(fstT, sndT, record)
}

object recordAcc2 {
  def apply(fstT: DataType, sndT: DataType, record: Phrase[AccType]): RecordAcc2 =
    RecordAcc2(fstT, sndT, record)
}

object skip extends Skip
