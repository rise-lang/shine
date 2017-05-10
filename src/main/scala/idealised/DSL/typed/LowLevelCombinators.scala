package idealised.DSL.typed

import lift.arithmetic.{NamedVar, RangeAdd}
import idealised.Core.TypeInference._
import idealised.Core._
import idealised.LowLevelCombinators._

object `new` {
  def apply(dt: DataType,
            addressSpace: AddressSpace,
            f: Phrase[(ExpType x AccType) -> CommandType]) =
    New(dt, addressSpace, f)

  def apply(dt: DataType,
            addressSpace: AddressSpace,
            f: Phrase[ExpType x AccType] => Phrase[CommandType]) =
    New(dt, addressSpace, λ(exp"[$dt]" x acc"[$dt]")( v => f(v) ))
}

object `if` {
  def apply[T <: PhraseType](cond: Phrase[ExpType],
                             thenP: Phrase[T],
                             elseP: Phrase[T]) =
    IfThenElsePhrase(cond, thenP, elseP)
}

object `for` {
  def apply(n: Nat,
            f: (Phrase[ExpType] => Phrase[CommandType])) =
    For(n, λ(exp"[idx($n)]")( i => f(i) ))
}

object `parFor` {
  def apply(n: Nat,
            dt: DataType,
            out: Phrase[AccType],
            f: (Phrase[ExpType] => Phrase[AccType] => Phrase[CommandType])) =
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
            C: Phrase[ExpType -> CommandType]) = {
    body match {
      case NatDependentLambdaPhrase(x, b) =>
        // ensure that the nested Nat dependent lambda has the proper range information
        val newX = NamedVar(x.name, RangeAdd(0, k, 1))
        val newB = PhraseType.substitute(newX, x, b)
        DoubleBufferFor(n, m, k, dt, addressSpace, buffer1, buffer2,
          NatDependentLambdaPhrase(newX, newB),
          C)
      case _ => throw new Exception("This should not happen")
    }
  }
}

object fst {
  def apply(record: Phrase[ExpType]) = {
    record.t match {
      case ExpType(RecordType(dt1, dt2)) => Fst(dt1, dt2, record)
      case x => error(x.toString, "ExpType(RecordType)")
    }
  }
}

object snd {
  def apply(record: Phrase[ExpType]) = {
    record.t match {
      case ExpType(RecordType(dt1, dt2)) => Snd(dt1, dt2, record)
      case x => error(x.toString, "ExpType(RecordType)")
    }
  }
}

object fstAcc {
  def apply(fstT: DataType, sndT: DataType, record: Phrase[AccType]) =
    FstAcc(fstT, sndT, record)
}

object sndAcc {
  def apply(fstT: DataType, sndT: DataType, record: Phrase[AccType]) =
    SndAcc(fstT, sndT, record)
}

object skip extends Skip
