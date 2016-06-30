import Core.PhraseType._
import Core._
import ExpPatterns._
import AccPatterns._
import CommandPatterns._
import Core.OperationalSemantics.{FloatData, IndexData, VectorData, newName}
import apart.arithmetic.ArithExpr

import scala.language.implicitConversions

package object DSL {
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

  implicit class BinOpsPatterns(lhs: ExpPattern) {
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
    def apply(arg: ArithExpr): Phrase[T] = {
      Lift.liftNatDependentFunction(fun)(arg)
    }

    def $(arg: ArithExpr): Phrase[T] = apply(arg)
  }

  implicit class FunComp[T1 <: PhraseType, T2 <: PhraseType](f: Phrase[T1 -> T2]) {
    def o[T3 <: PhraseType](g: Phrase[T3 -> T1]): Phrase[T3 -> T2] = {
      val param = IdentPhrase[T3](newName(), null.asInstanceOf[T3])
      λ(param)((arg: IdentPhrase[T3]) => f(g(arg)))
    }
  }

  implicit class SequentialComposition(c1: Phrase[CommandType]) {
    def `;`(c2: Phrase[CommandType]): Phrase[CommandType] = Seq(c1, c2)
  }

  implicit class SequentialPatternComposition(c1: IntermediateCommandPattern) {
    def `;`(c2: Phrase[CommandType]): Phrase[CommandType] = Seq(c1, c2)
  }

  implicit class Assignment(lhs: Phrase[AccType]) {
    def :=(rhs: Phrase[ExpType]) = Assign(lhs, rhs)
  }

  implicit class AssignmentPattern(lhs: AccPattern) {
    def :=(rhs: Phrase[ExpType]) = Assign(lhs, rhs)
  }

  implicit def toPair[T1 <: PhraseType, T2 <: PhraseType](pair: (Phrase[T1], Phrase[T2])): PairPhrase[T1, T2] = {
    PairPhrase(pair._1, pair._2)
  }

  implicit def toLiteralIndex(i: ArithExpr): LiteralPhrase = LiteralPhrase(IndexData(i))

  implicit def toLiteralInt(i: Int): LiteralPhrase = LiteralPhrase(i)
  implicit def toLiteralFloat(f: Float): LiteralPhrase = LiteralPhrase(FloatData(f))
  implicit def toLiteralFloat4(v: VectorData): LiteralPhrase = LiteralPhrase(v)

  implicit def toNatDependentLambda[T <: PhraseType](p: Phrase[T]): NatDependentLambdaPhrase[T] = _Λ_( l => p )

  implicit class ExpPhraseExtensions(e: Phrase[ExpType]) {
    def _1 = Fst(e)

    def _2 = Snd(e)

    def `@`(index: Phrase[ExpType]) = {
      e.t match {
        case ExpType(ArrayType(n, dt)) => Idx(n, dt, index, e)
        case null => Idx(null, null, index, e)
        case x => throw new Exception(s"This should not happen: $x")
      }
    }
  }

  implicit class AccPhraseExtensions(a: Phrase[AccType]) {
    def `@`(index: Phrase[ExpType]) = {
      a.t match {
        case AccType(ArrayType(n, dt)) => IdxAcc(n, dt, index, a)
        case null => IdxAcc(null, null, index, a)
        case x => throw new Exception(s"This should not happen: $x")
      }
    }
  }

  implicit class VarExtensions(v: Phrase[VarType]) {
    def rd: Proj1Phrase[ExpType, AccType] = π1(v)
    def wr: Proj2Phrase[ExpType, AccType] = π2(v)
  }
}
