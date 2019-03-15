package idealised.SurfaceLanguage

import idealised.SurfaceLanguage.Primitives.{Fst, Snd, Zip}
import idealised.SurfaceLanguage.Semantics._
import idealised.SurfaceLanguage.Types._
import lift.arithmetic.{ContinuousRange, NamedVar}

import scala.language.implicitConversions

package object DSL {

  implicit class BinOps(lhs: Expr) {
    def +(rhs: Expr) = BinOpExpr(Operators.Binary.ADD, lhs, rhs)
    def -(rhs: Expr) = BinOpExpr(Operators.Binary.SUB, lhs, rhs)
    def *(rhs: Expr) = BinOpExpr(Operators.Binary.MUL, lhs, rhs)
    def /(rhs: Expr) = BinOpExpr(Operators.Binary.DIV, lhs, rhs)
    def %(rhs: Expr) = BinOpExpr(Operators.Binary.MOD, lhs, rhs)
    def >(rhs: Expr) = BinOpExpr(Operators.Binary.GT, lhs, rhs)
    def <(rhs: Expr) = BinOpExpr(Operators.Binary.LT, lhs, rhs)
    def =:=(rhs: Expr) = BinOpExpr(Operators.Binary.EQ, lhs, rhs)
    def unary_- = UnaryOpExpr(Operators.Unary.NEG, lhs)
  }

  implicit class FunCall(f: Expr) {
    def apply(arg: Expr): Expr = Lifting.liftFunctionExpr(f)(arg)
    def $(arg: Expr): Expr = apply(arg)

    def apply(arg: Nat): Expr = Lifting.liftNatDependentFunctionExpr(f)(arg)
    def $(arg: Nat): Expr = apply(arg)

    def apply(arg: DataType): Expr = Lifting.liftTypeDependentFunctionExpr(f)(arg)
    def $(arg: DataType): Expr = apply(arg)
  }

  implicit class FunCallExpr(arg: Expr) {
    def :>>(f: Expr): Expr = f(arg)
    def <<:(f: Expr): Expr = f(arg)
  }

  implicit class FunCallExprPair(args: (Expr, Expr)) {
    def :>>(z: (Expr, Expr) => Zip): Zip = z(args._1, args._2)
    def <<:(z: (Expr, Expr) => Zip): Zip = z(args._1, args._2)
  }

  implicit class CallTypeDependentLambdaExpr(arg: DataType) {
    def :>>(f: Expr): Expr = f(arg)
    def <<:(f: Expr): Expr = f(arg)
  }

  implicit class FunComp(f: Expr) {
    def o(g: Expr): Expr = fun(arg => f( g(arg) ) )
    def <<<(g: Expr): Expr = f o g
  }

  implicit class RevFunComp(f: Expr) {
    def >>>(g: Expr): Expr = g o f
  }

  implicit def l(i: Int): LiteralExpr = LiteralExpr(IntData(i))
  implicit def l(f: Float): LiteralExpr = LiteralExpr(FloatData(f))
  implicit def l(v: VectorData): LiteralExpr = LiteralExpr(v)

  implicit class IdentExpPhraseExtensions(i: IdentifierExpr) {
    def asNatIdentifier = NamedVar(i.name)
    def asNatIdentifier(withUpperBound: Nat) = NamedVar(i.name, ContinuousRange(0, withUpperBound))
  }

  case class NatExtensions(n: Nat) {
    def asExpr = LiteralExpr(IndexData(n))
    def asExpr(withType: IndexType) = LiteralExpr(IndexData(n, withType))
  }

  implicit class ExpPhraseExtensions(e: Expr) {
    def _1 = Fst(e, None)
    def _2 = Snd(e, None)
  }
}
