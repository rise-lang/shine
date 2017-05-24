package idealised.SurfaceLanguage.DSL

import idealised.utils._
import idealised.DPIA.Types.PhraseType
import idealised.DPIA.ImperativePrimitives._
import idealised.SurfaceLanguage.{Expr, IfThenElseExpr}

object `if` {
  def apply[T <: PhraseType](cond: DataExpr,
                             thenP: Expr[T],
                             elseP: Expr[T]): IfThenElseExpr[T] =
    IfThenElseExpr(cond, thenP, elseP)
}

//object `for` {
//  def apply(n: Nat, f: (Phrase[ExpType] => Phrase[CommandType])): For =
//    For(n, λ(exp"[idx($n)]")( i => f(i) ))
//}
//
//object `parFor` {
//  def apply(n: Nat,
//            dt: DataType,
//            out: Phrase[AccType],
//            f: (Phrase[ExpType] => Phrase[AccType] => Phrase[CommandType])): ParFor =
//    ParFor(n, dt, out, λ(exp"[$int]")( i => λ(acc"[$dt]")( o => f(i)(o) )))
//}
//
//object dblBufFor {
//  def apply(n: Nat,
//            m: Nat,
//            k: Nat,
//            dt: DataType,
//            addressSpace: AddressSpace,
//            buffer1: Phrase[VarType],
//            buffer2: Phrase[VarType],
//            body: Phrase[`(nat)->`[AccType -> (ExpType -> CommandType)]],
//            C: Phrase[ExpType -> CommandType]): DoubleBufferFor =
//    DoubleBufferFor(n, m, k, dt, addressSpace, buffer1, buffer2, body, C)
//}
//
//object `new` {
//  def apply(dt: DataType,
//            addressSpace: AddressSpace,
//            f: Phrase[(ExpType x AccType) -> CommandType]): New =
//    New(dt, addressSpace, f)
//
//  def apply(dt: DataType,
//            addressSpace: AddressSpace,
//            f: Phrase[ExpType x AccType] => Phrase[CommandType]): New =
//    New(dt, addressSpace, λ(exp"[$dt]" x acc"[$dt]")( v => f(v) ))
//}
//
//object fst {
//  def apply(record: Phrase[ExpType]): Fst = Fst(null, null, record)
//}
//
//object snd {
//  def apply(record: Phrase[ExpType]): Snd = Snd(null, null, record)
//}
//
//object recordAcc1 {
//  def apply(fstT: DataType, sndT: DataType, record: Phrase[AccType]): RecordAcc1 =
//    RecordAcc1(fstT, sndT, record)
//}
//
//object recordAcc2 {
//  def apply(fstT: DataType, sndT: DataType, record: Phrase[AccType]): RecordAcc2 =
//    RecordAcc2(fstT, sndT, record)
//}

object skip extends Skip
