package DSL

import Core.OperationalSemantics.IndexData
import Core.TypeInference._
import Core._
import LowLevelCombinators.{Assign, Idx, IdxAcc, Seq}
import apart.arithmetic.ArithExpr

import scala.language.implicitConversions

package object typed {

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

  implicit class ExpPhraseExtensions(e: Phrase[ExpType]) {
    def `@`(index: Phrase[ExpType]) = e.t match {
      case ExpType(ArrayType(n, dt)) => Idx(n, dt, index, e)
      case x => error(x.toString, "exp[n.dt]")
    }
  }

  implicit class AccPhraseExtensions(a: Phrase[AccType]) {
    def `@`(index: Phrase[ExpType]) = a.t match {
      case AccType(ArrayType(n, dt)) => IdxAcc(n, dt, index, a)
      case x => error(x.toString, "acc[n.dt]")
    }
  }

  implicit class Assignment(lhs: Phrase[AccType]) {
    def :=(rhs: Phrase[ExpType]) = {
      (lhs.t, rhs.t) match {
        case (AccType(dt1), ExpType(dt2))
          if dt1 == dt2 && dt1.isInstanceOf[BasicType] =>
          Assign(dt1.asInstanceOf[BasicType], lhs, rhs)
        case (x1, x2) => error(x1.toString() + " and " + x2.toString(),
          expected = "them to have a matching data type.")
      }
    }
  }

  implicit class CallLambda[T1 <: PhraseType, T2 <: PhraseType](fun: Phrase[T1 -> T2]) {
    def apply(arg: Phrase[T1]): Phrase[T2] = Lift.liftFunction(fun)(arg)

    def $(arg: Phrase[T1]): Phrase[T2] = apply(arg)
  }

  implicit class CallNatDependentLambda[T <: PhraseType](fun: Phrase[`(nat)->`[T]]) {
    def apply(arg: ArithExpr): Phrase[T] =
      Lift.liftNatDependentFunction(fun)(arg)

    def $(arg: ArithExpr): Phrase[T] = apply(arg)
  }

  implicit class FunComp[T1 <: PhraseType, T2 <: PhraseType](f: Phrase[T1 -> T2]) {
    def o[T3 <: PhraseType](g: Phrase[T3 -> T1]): Phrase[T3 -> T2] = {
      typed.λ(g.t.inT)(arg => f(g(arg)))
    }
  }

  implicit class SequentialComposition(c1: Phrase[CommandType]) {
    def `;`(c2: Phrase[CommandType]): Phrase[CommandType] = Seq(c1, c2)
  }

  implicit class VarExtensions(v: Phrase[VarType]) {
    def rd: Proj1Phrase[ExpType, AccType] = π1(v)

    def wr: Proj2Phrase[ExpType, AccType] = π2(v)
  }

  implicit def toLiteralIndex(i: ArithExpr): LiteralPhrase = LiteralPhrase(IndexData(i))

  implicit def toLiteralInt(i: Int): LiteralPhrase = LiteralPhrase(i)

  implicit def toPair[T1 <: PhraseType, T2 <: PhraseType](pair: (Phrase[T1], Phrase[T2])): PairPhrase[T1, T2] =
    PairPhrase(pair._1, pair._2)

  implicit def toNatDependentLambda[T <: PhraseType](p: Phrase[T]): NatDependentLambdaPhrase[T] =
    _Λ_( l => p )
}
