package idealised.SurfaceLanguage

import idealised.SurfaceLanguage.Primitives.{Fst, Snd}
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
    def unary_- = UnaryOpExpr(Operators.Unary.NEG, lhs)
  }

  implicit class CallLambdaExpr[T <: Type](fun: Expr[DataType -> T]) {
    def apply(arg: DataExpr): Expr[T] = Lifting.liftFunctionExpr(fun)(arg)

    def $(arg: DataExpr): Expr[T] = apply(arg)
  }

  implicit class CallNatDependentLambdaExpr[T <: Type](fun: Expr[`(nat)->`[T]]) {
    def apply(arg: Nat): Expr[T] = Lifting.liftNatDependentFunctionExpr(fun)(arg)

    def $(arg: Nat): Expr[T] = apply(arg)
  }

  implicit class CallTypeDependentLambdaExpr[T <: Type](fun: Expr[`(dt)->`[T]]) {
    def apply(arg: DataType): Expr[T] = Lifting.liftTypeDependentFunctionExpr(fun)(arg)

    def $(arg: DataType): Expr[T] = apply(arg)
  }

  implicit class FunComp[T <: Type](f: Expr[DataType -> T]) {
    def o(g: Expr[DataType -> DataType]): Expr[DataType -> T] = {
      λ(arg => f( g(arg) ) )
    }
  }

  implicit def toLiteralInt(i: Int): LiteralExpr = LiteralExpr(IntData(i), int)
  implicit def toLiteralFloat(f: Float): LiteralExpr = LiteralExpr(FloatData(f), float)
  implicit def toLiteralFloat4(v: VectorData): LiteralExpr =
    LiteralExpr(v, VectorType(v.v.length, v.v.head.dataType))

  implicit def toNatDependentLambda[T <: Type](p: Expr[T]): NatDependentLambdaExpr[T] =
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
  }
}
