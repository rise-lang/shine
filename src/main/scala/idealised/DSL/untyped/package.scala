package idealised.DSL

import idealised.Core._
import idealised.Core.OperationalSemantics.{FloatData, IndexData, VectorData}
import idealised.ImperativePrimitives._
import idealised.DSL.untyped.FunctionalPrimitives.{Fst, Snd}
import lift.arithmetic.{ContinuousRange, NamedVar}

import scala.language.implicitConversions

package object untyped {
  type DataExpr = Expr[ExpType]

  implicit class BinOps(lhs: DataExpr) {
    def +(rhs: DataExpr) = BinOpExpr(BinOp.Op.ADD, lhs, rhs)
    def -(rhs: DataExpr) = BinOpExpr(BinOp.Op.SUB, lhs, rhs)
    def *(rhs: DataExpr) = BinOpExpr(BinOp.Op.MUL, lhs, rhs)
    def /(rhs: DataExpr) = BinOpExpr(BinOp.Op.DIV, lhs, rhs)
    def %(rhs: DataExpr) = BinOpExpr(BinOp.Op.MOD, lhs, rhs)
    def >(rhs: DataExpr) = BinOpExpr(BinOp.Op.GT, lhs, rhs)
    def <(rhs: DataExpr) = BinOpExpr(BinOp.Op.LT, lhs, rhs)
    def unary_- = UnaryOpExpr(UnaryOp.Op.NEG, lhs)
  }

  implicit class CallLambdaExpr[T <: PhraseType](fun: Expr[ExpType -> T]) {
    def apply(arg: DataExpr): Expr[T] = Lifting.liftFunctionExpr(fun)(arg)

    def $(arg: DataExpr): Expr[T] = apply(arg)
  }

  implicit class CallNatDependentLambdaExpr[T <: PhraseType](fun: Expr[`(nat)->`[T]]) {
    def apply(arg: Nat): Expr[T] = Lifting.liftNatDependentFunctionExpr(fun)(arg)

    def $(arg: Nat): Expr[T] = apply(arg)
  }

  implicit class CallTypeDependentLambdaExpr[T <: PhraseType](fun: Expr[`(dt)->`[T]]) {
    def apply(arg: DataType): Expr[T] = Lifting.liftTypeDependentFunctionExpr(fun)(arg)

    def $(arg: DataType): Expr[T] = apply(arg)
  }

  implicit class FunComp[T <: PhraseType](f: Expr[ExpType -> T]) {
    def o(g: Expr[ExpType -> ExpType]): Expr[ExpType -> T] = {
      λ(arg => f( g(arg) ) )
    }
  }

//  implicit class SequentialComposition(c1: Phrase[CommandType]) {
//    def `;`(c2: Phrase[CommandType]): Phrase[CommandType] = Seq(c1, c2)
//  }
//
//  implicit class Assignment(lhs: Phrase[AccType]) {
//    def :=(rhs: Phrase[ExpType]): Assign = {
//      (lhs.t, rhs.t) match {
//        case (AccType(dt1), ExpType(dt2))
//          if dt1 == dt2 && dt1.isInstanceOf[BasicType] =>
//          Assign(dt1.asInstanceOf[BasicType], lhs, rhs)
//        case _ => Assign(null, lhs, rhs)
//      }
//    }
//  }
//
//  implicit def toPair[T1 <: PhraseType, T2 <: PhraseType](pair: (Phrase[T1], Phrase[T2])): Pair[T1, T2] =
//    Pair(pair._1, pair._2)

  implicit def toLiteralInt(i: Int): LiteralExpr = LiteralExpr(i, int)
  implicit def toLiteralFloat(f: Float): LiteralExpr = LiteralExpr(FloatData(f), float)
  implicit def toLiteralFloat4(v: VectorData): LiteralExpr =
    LiteralExpr(v, VectorType(v.a.length, v.a.head.dataType.asInstanceOf[ScalarType]))

  implicit def toNatDependentLambda[T <: PhraseType](p: Expr[T]): NatDependentLambdaExpr[T] =
    _Λ_( _ => p )

  implicit class IdentExpPhraseExtensions(i: IdentifierExpr) {
    def asNatIdentifier = NamedVar(i.name)
    def asNatIdentifier(withUpperBound: Nat) =
      NamedVar(i.name, ContinuousRange(0, withUpperBound))
  }

  implicit class NatExtensions(n: Nat) {
    def asExpr = LiteralExpr(IndexData(n), IndexType(n.max))
    def asExpr(withType: IndexType) = LiteralExpr(IndexData(n), withType)
  }

  implicit class ExpPhraseExtensions(e: DataExpr) {
    def _1 = Fst(e)
    def _2 = Snd(e)

//    def `@`(index: Phrase[ExpType]): Idx = (index.t, e.t) match {
//      case (ExpType(IndexType(n)), ExpType(ArrayType(n_, dt))) if n == n_ =>
//        Idx(n, dt, index, e)
//      case _ => Idx(null, null, index, e)
//    }

  }

//  implicit class AccPhraseExtensions(a: Phrase[AccType]) {
//    def `@`(index: Phrase[ExpType]): IdxAcc = (index.t, a.t) match {
//      case (ExpType(IndexType(n)), AccType(ArrayType(n_, dt))) if n == n_ =>
//        IdxAcc(n, dt, index, a)
//      case _ => IdxAcc(null, null, index, a)
//    }
//  }

//  implicit class VarExtensions(v: Phrase[VarType]) {
//    def rd: Proj1[ExpType, AccType] = π1(v)
//    def wr: Proj2[ExpType, AccType] = π2(v)
//  }
}
