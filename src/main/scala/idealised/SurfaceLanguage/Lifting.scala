package idealised.SurfaceLanguage

import idealised.SurfaceLanguage.Primitives.{IndexAsNat, AsIndex}
import idealised.SurfaceLanguage.Semantics.IndexData
import idealised.SurfaceLanguage.Types._
import lift.arithmetic.{NamedVar, RangeAdd, StartFromRange}

import scala.language.postfixOps
import scala.language.reflectiveCalls

object Lifting {

  def liftFunctionExpr(p: Expr): Expr => Expr = {
    p match {
      case l: LambdaExpr =>
        (arg: Expr) =>  Expr.substitute(arg, `for` = l.param, in = l.body)
      case app: ApplyExpr =>
        val fun = liftFunctionExpr(app.fun)
        liftFunctionExpr(fun(app.arg))
      case app: NatDependentApplyExpr =>
        val fun = liftNatDependentFunctionExpr(app.fun)
        liftFunctionExpr(fun(app.arg))
      case app: TypeDependentApplyExpr =>
        val fun = liftTypeDependentFunctionExpr(app.fun)
        liftFunctionExpr(fun(app.arg))
      case _: IfThenElseExpr =>
        throw new Exception("This should never happen")
    }
  }

  def liftNatDependentFunctionExpr(p: Expr): Nat => Expr = {
    p match {
      case l: NatDependentLambdaExpr =>
        (arg: Nat) => Type.substitute(arg, `for` = l.x, in = l.body)
      case app: ApplyExpr =>
        val fun = liftFunctionExpr(app.fun)
        liftNatDependentFunctionExpr(fun(app.arg))
      case app: NatDependentApplyExpr =>
        val fun = liftNatDependentFunctionExpr(app.fun)
        liftNatDependentFunctionExpr(fun(app.arg))
      case app: TypeDependentApplyExpr =>
        val fun = liftTypeDependentFunctionExpr(app.fun)
        liftNatDependentFunctionExpr(fun(app.arg))
      case _: IfThenElseExpr =>
        throw new Exception("This should never happen")
    }
  }

  def liftTypeDependentFunctionExpr(p: Expr): DataType => Expr = {
    p match {
      case l: TypeDependentLambdaExpr =>
        (arg: DataType) => Type.substitute(arg, `for` = l.x, in = l.body)
      case app: ApplyExpr =>
        val fun = liftFunctionExpr(app.fun)
        liftTypeDependentFunctionExpr(fun(app.arg))
      case app: NatDependentApplyExpr =>
        val fun = liftNatDependentFunctionExpr(app.fun)
        liftTypeDependentFunctionExpr(fun(app.arg))
      case app: TypeDependentApplyExpr =>
        val fun = liftTypeDependentFunctionExpr(app.fun)
        liftTypeDependentFunctionExpr(fun(app.arg))
      case _: IfThenElseExpr =>
        throw new Exception("This should never happen")
    }
  }
}
