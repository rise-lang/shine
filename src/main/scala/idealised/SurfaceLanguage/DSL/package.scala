package idealised.SurfaceLanguage

import idealised.SurfaceLanguage.Primitives.{Fst, Snd, Zip}
import idealised.SurfaceLanguage.Semantics._
import idealised.SurfaceLanguage.Types._
import lift.arithmetic.{ContinuousRange, NamedVar}

import scala.language.implicitConversions

package object DSL {
  type DataExpr = Expr[DataType]

  implicit class BinOps(lhs: DataExpr) {
    def +(rhs: DataExpr) = BinOpExpr(Operators.Binary.ADD, lhs, rhs)
    def -(rhs: DataExpr) = BinOpExpr(Operators.Binary.SUB, lhs, rhs)
    def *(rhs: DataExpr) = BinOpExpr(Operators.Binary.MUL, lhs, rhs)
    def /(rhs: DataExpr) = BinOpExpr(Operators.Binary.DIV, lhs, rhs)
    def %(rhs: DataExpr) = BinOpExpr(Operators.Binary.MOD, lhs, rhs)
    def >(rhs: DataExpr) = BinOpExpr(Operators.Binary.GT, lhs, rhs)
    def <(rhs: DataExpr) = BinOpExpr(Operators.Binary.LT, lhs, rhs)
    def =:=(rhs: DataExpr) = BinOpExpr(Operators.Binary.EQ, lhs, rhs)
    def unary_- = UnaryOpExpr(Operators.Unary.NEG, lhs)
  }

  implicit class FunCall[T <: Type](f: Expr[DataType -> T]) {
    def apply(arg: DataExpr): Expr[T] = Lifting.liftFunctionExpr(f)(arg)
    def $(arg: DataExpr): Expr[T] = apply(arg)
  }

  implicit class FunCallExpr(arg: DataExpr) {
    def :>>[T <: Type](f: Expr[DataType -> T]): Expr[T] = f(arg)
    def <<:[T <: Type](f: Expr[DataType -> T]): Expr[T] = f(arg)
  }

  implicit class FunCallExprPair(args: (Expr[DataType], Expr[DataType])) {
    def :>>(z: (DataExpr, DataExpr) => Zip): Zip = z(args._1, args._2)
    def <<:(z: (DataExpr, DataExpr) => Zip): Zip = z(args._1, args._2)
  }

  implicit class CallNatDependentLambda[T <: Type](f: Expr[`(nat)->`[T]]) {
    def apply(arg: Nat): Expr[T] = Lifting.liftNatDependentFunctionExpr(f)(arg)
    def $(arg: Nat): Expr[T] = apply(arg)
  }

  implicit class CallNatDependentLambdaExpr(arg: Nat) {
    def :>>[T <: Type](f: Expr[`(nat)->`[T]]): Expr[T] = f(arg)
    def <<:[T <: Type](f: Expr[`(nat)->`[T]]): Expr[T] = f(arg)
  }

  implicit class CallTypeDependentLambda[T <: Type](f: Expr[`(dt)->`[T]]) {
    def apply(arg: DataType): Expr[T] = Lifting.liftTypeDependentFunctionExpr(f)(arg)
    def $(arg: DataType): Expr[T] = apply(arg)
  }

  implicit class CallTypeDependentLambdaExpr(arg: DataType) {
    def :>>[T <: Type](f: Expr[`(dt)->`[T]]): Expr[T] = f(arg)
    def <<:[T <: Type](f: Expr[`(dt)->`[T]]): Expr[T] = f(arg)
  }

  implicit class FunComp[T <: Type](f: Expr[DataType -> T]) {
    def o(g: Expr[DataType -> DataType]): Expr[DataType -> T] = fun(arg => f( g(arg) ) )
    def <<<(g: Expr[DataType -> DataType]): Expr[DataType -> T] = f o g
  }

  implicit class RevFunComp(f: Expr[DataType -> DataType]) {
    def >>>[T <: Type](g: Expr[DataType -> T]): Expr[DataType -> T] = g o f
  }

  implicit def toLiteralInt(i: Int): LiteralExpr = LiteralExpr(IntData(i))
  implicit def toLiteralFloat(f: Float): LiteralExpr = LiteralExpr(FloatData(f))
  implicit def toLiteralFloatN(v: VectorData): LiteralExpr = LiteralExpr(v)

  implicit def toNatDependentLambda[T <: Type](p: Expr[T]): NatDependentLambdaExpr[T] =
    nFun(_ => p)

  implicit class IdentExpPhraseExtensions(i: IdentifierExpr) {
    def asNatIdentifier: Nat = NamedVar(i.name)
    def asNatIdentifier(withUpperBound: Nat): Nat = NamedVar(i.name, ContinuousRange(0, withUpperBound))
  }

  def toNatIdentifier(i: IdentifierExpr): NamedVar = i.t match {
    case Some(IndexType(n)) => NamedVar(i.name, ContinuousRange(0, n))
    case _ => throw new Exception(s"Couldn't convert identifier ${i.name} with type ${i.t} to Nat.")
  }

  implicit class NatExtensions(n: Nat) {
    def asExpr = LiteralExpr(IndexData(n))
    def asExpr(withType: IndexType) = LiteralExpr(IndexData(n, withType))
  }

  implicit class ExpPhraseExtensions(e: DataExpr) {
    def _1 = Fst(e, None)
    def _2 = Snd(e, None)
  }

//  implicit class ExpPhraseExtensions(e: DataExpr) {
//    def `@`(index: DataExpr): Idx = (index.t, e.t) match {
//      case (ExpType(IndexType(n1)), ExpType(ArrayType(n2, dt))) if n1 == n2 =>
//        Idx(n1, dt, index, e)
//      case x => error(x.toString, "(exp[idx(n)], exp[n.dt])")
//    }
//  }
}
