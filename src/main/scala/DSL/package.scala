import Core.PhraseType._
import Core._

import scala.language.implicitConversions

package object DSL {
  implicit class BinOps(lhs: Phrase[ExpType]) {
    def +(rhs: Phrase[ExpType]) = BinOp(BinOp.Op.ADD, lhs, rhs)

    def -(rhs: Phrase[ExpType]) = BinOp(BinOp.Op.SUB, lhs, rhs)

    def *(rhs: Phrase[ExpType]) = BinOp(BinOp.Op.MUL, lhs, rhs)

    def /(rhs: Phrase[ExpType]) = BinOp(BinOp.Op.DIV, lhs, rhs)

    def %(rhs: Phrase[ExpType]) = BinOp(BinOp.Op.MOD, lhs, rhs)
  }

  implicit class CallLambda[T1 <: PhraseType, T2 <: PhraseType](lambda: Lambda[T1, T2]) {
    def apply(arg: Phrase[T1]) = Apply(lambda, arg)
  }

  implicit class SequentialComposition(c1: Phrase[CommandType]) {
    def `;`(c2: Phrase[CommandType]) = Seq(c1, c2)
  }

  implicit class Assignment(lhs: Phrase[AccType]) {
    def :=(rhs: Phrase[ExpType]) = Assign(lhs, rhs)
  }

  implicit def toPair[T1 <: PhraseType, T2 <: PhraseType](pair: (Phrase[T1], Phrase[T2])): Pair[T1, T2] = {
    Pair(pair._1, pair._2)
  }

  implicit def toLiteral(i: Int): Literal = Literal(i)

  implicit class ExpPhraseExtensions(e: Phrase[ExpType]) {
    def _1() = FieldAccess(0, e)

    def _2() = FieldAccess(1, e)

    def `@`(index: Phrase[ExpType]) = ArrayExpAccessPhrase(e, index)
  }

  implicit class PatternExtensions(p: Pattern) {
    def _1() = FieldAccess(0, p)

    def _2() = FieldAccess(1, p)

    def `@`(index: Phrase[ExpType]) = ArrayExpAccessPhrase(p, index)
  }

  implicit class AccPhraseExtensions(a: Phrase[AccType]) {
    def `@`(index: Phrase[ExpType]) = ArrayAccAccessPhrase(a, index)
  }

  implicit class VarExtensions(v: Phrase[VarType]) {
    def exp = π1(v)
    def acc = π2(v)
  }

  implicit def toPatternPhrase(p: Pattern): PatternPhrase = PatternPhrase(p)
}
