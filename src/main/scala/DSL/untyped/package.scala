package DSL

import Core._
import Core.OperationalSemantics.{FloatData, IndexData, VectorData}
import LowLevelCombinators._
import apart.arithmetic.NamedVar

import scala.language.implicitConversions

package object untyped {
  implicit class BinOps(lhs: Phrase[ExpType]) {
    def +(rhs: Phrase[ExpType]) = BinOpPhrase(BinOpPhrase.Op.ADD, lhs, rhs)
    def -(rhs: Phrase[ExpType]) = BinOpPhrase(BinOpPhrase.Op.SUB, lhs, rhs)
    def *(rhs: Phrase[ExpType]) = BinOpPhrase(BinOpPhrase.Op.MUL, lhs, rhs)
    def /(rhs: Phrase[ExpType]) = BinOpPhrase(BinOpPhrase.Op.DIV, lhs, rhs)
    def %(rhs: Phrase[ExpType]) = BinOpPhrase(BinOpPhrase.Op.MOD, lhs, rhs)
    def >(rhs: Phrase[ExpType]) = BinOpPhrase(BinOpPhrase.Op.GT, lhs, rhs)
    def <(rhs: Phrase[ExpType]) = BinOpPhrase(BinOpPhrase.Op.LT, lhs, rhs)
    def unary_- = UnaryOpPhrase(UnaryOpPhrase.Op.NEG, lhs)
  }

  implicit class CallLambda[T1 <: PhraseType, T2 <: PhraseType](fun: Phrase[T1 -> T2]) {
    def apply(arg: Phrase[T1]): Phrase[T2] = Lift.liftFunction(fun)(arg)

    def $(arg: Phrase[T1]): Phrase[T2] = apply(arg)
  }

  implicit class CallNatDependentLambda[T <: PhraseType](fun: Phrase[`(nat)->`[T]]) {
    def apply(arg: Nat): Phrase[T] =
      Lift.liftNatDependentFunction(fun)(arg)

    def $(arg: Nat): Phrase[T] = apply(arg)
  }

  implicit class CallTypeDependentLambda[T <: PhraseType](fun: Phrase[`(dt)->`[T]]) {
    def apply(arg: DataType): Phrase[T] =
      Lift.liftTypeDependentFunction(fun)(arg)

    def $(arg: DataType): Phrase[T] = apply(arg)
  }

  implicit class FunComp[T1 <: PhraseType, T2 <: PhraseType](f: Phrase[T1 -> T2]) {
    def o[T3 <: PhraseType](g: Phrase[T3 -> T1]): Phrase[T3 -> T2] = {
      λ(null.asInstanceOf[T3])(arg => f(g(arg)))
    }
  }

  implicit class SequentialComposition(c1: Phrase[CommandType]) {
    def `;`(c2: Phrase[CommandType]): Phrase[CommandType] = Seq(c1, c2)
  }

  implicit class Assignment(lhs: Phrase[AccType]) {
    def :=(rhs: Phrase[ExpType]) = {
      (lhs.t, rhs.t) match {
        case (AccType(dt1), ExpType(dt2))
          if dt1 == dt2 && dt1.isInstanceOf[BasicType] =>
          Assign(dt1.asInstanceOf[BasicType], lhs, rhs)
        case _ => Assign(null, lhs, rhs)
      }
    }
  }

  implicit def toPair[T1 <: PhraseType, T2 <: PhraseType](pair: (Phrase[T1], Phrase[T2])): PairPhrase[T1, T2] =
    PairPhrase(pair._1, pair._2)

  implicit def toLiteralInt(i: Int): LiteralPhrase = LiteralPhrase(i, int)
  implicit def toLiteralFloat(f: Float): LiteralPhrase = LiteralPhrase(FloatData(f), float)
  implicit def toLiteralFloat4(v: VectorData): LiteralPhrase =
    LiteralPhrase(v, VectorType(v.a.length, v.a.head.dataType.asInstanceOf[BasicType]))

  implicit def toNatDependentLambda[T <: PhraseType](p: Phrase[T]): NatDependentLambdaPhrase[T] =
    _Λ_( l => p )

  implicit class ExpPhraseExtensions(e: Phrase[ExpType]) {
    def _1 = Fst(null, null, e)
    def _2 = Snd(null, null, e)

    def `@`(index: Phrase[ExpType]) = (index.t, e.t) match {
      case (ExpType(IndexType(n)), ExpType(ArrayType(n_, dt))) if n == n_ =>
        Idx(n, dt, index, e)
      case _ => Idx(null, null, index, e)
    }

  }

  implicit class AccPhraseExtensions(a: Phrase[AccType]) {
    def `@`(index: Phrase[ExpType]) = (index.t, a.t) match {
      case (ExpType(IndexType(n)), AccType(ArrayType(n_, dt))) if n == n_ =>
        IdxAcc(n, dt, index, a)
      case _ => IdxAcc(null, null, index, a)
    }
  }

  implicit class VarExtensions(v: Phrase[VarType]) {
    def rd: Proj1Phrase[ExpType, AccType] = π1(v)
    def wr: Proj2Phrase[ExpType, AccType] = π2(v)
  }
}
