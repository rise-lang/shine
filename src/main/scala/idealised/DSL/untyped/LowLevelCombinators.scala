package idealised.DSL.untyped

import idealised.Core._
import idealised.LowLevelCombinators._

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
    ParFor(n, dt, out, λ(exp"[$int]")( i => λ(acc"[$dt]")( o => f(i)(o) )))
}

object dblBufFor {
  def apply(n: Nat,
            dt: DataType,
            addressSpace: AddressSpace,
            buffer1: Phrase[VarType],
            buffer2: Phrase[VarType],
            k: Nat,
            body: Phrase[`(nat)->`[AccType -> (ExpType -> CommandType)]],
            C: Phrase[ExpType -> CommandType]) =
    DoubleBufferFor(n, dt, addressSpace, buffer1, buffer2, k, body, C)
}

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

object fst {
  def apply(record: Phrase[ExpType]) = Fst(null, null, record)
}

object snd {
  def apply(record: Phrase[ExpType]) = Snd(null, null, record)
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
