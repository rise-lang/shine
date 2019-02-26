package idealised.DPIA.DSL

import idealised.DPIA.FunctionalPrimitives.{Fst, Snd}
import idealised.DPIA.ImperativePrimitives._
import idealised.DPIA.Phrases.{Identifier, IfThenElse, NatDependentLambda, Phrase}
import idealised.DPIA.Types._
import idealised.DPIA._
import lift.arithmetic.{NamedVar, RangeAdd}

object `new` {
  def apply(dt: DataType,
            addressSpace: AddressSpace,
            f: Phrase[VarType -> CommandType]): New =
    New(dt, addressSpace, f)

  def apply(dt: DataType,
            addressSpace: AddressSpace,
            f: Phrase[VarType] => Phrase[CommandType]): New =
    New(dt, addressSpace, λ(exp"[$dt]" x acc"[$dt]")( v => f(v) ))
}

object newDoubleBuffer {
  def apply(dt1: DataType,
            dt2: DataType,
            dt3: ArrayType,
            in: Phrase[ExpType],
            out: Phrase[AccType],
            f: Phrase[VarType x CommandType x CommandType -> CommandType]): NewDoubleBuffer =
    NewDoubleBuffer(dt1, dt2, dt3.elemType, dt3.size, in, out, f)

  def apply(dt1: DataType,
            dt2: DataType,
            dt3: ArrayType,
            in: Phrase[ExpType],
            out: Phrase[AccType],
            f: (Phrase[VarType], Phrase[CommandType], Phrase[CommandType]) => Phrase[CommandType]) =
    NewDoubleBuffer(dt1, dt2, dt3.elemType, dt3.size, in, out, λ(VarType(dt1) x CommandType() x CommandType())(ps => {
      val    v: Phrase[VarType]     = ps._1._1
      val swap: Phrase[CommandType] = ps._1._2
      val done: Phrase[CommandType] = ps._2
      f(v, swap, done)
    }))
}

object `if` {
  def apply[T <: PhraseType](cond: Phrase[ExpType],
                             thenP: Phrase[T],
                             elseP: Phrase[T]): IfThenElse[T] =
    IfThenElse(cond, thenP, elseP)

  //noinspection TypeAnnotation
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
            f: Identifier[ExpType] => Phrase[CommandType], unroll:Boolean = false): For =
    For(n, λ(exp"[idx($n)]")( i => f(i) ), unroll)
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
