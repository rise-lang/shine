package idealised.SurfaceLanguage

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Types._

import scala.language.postfixOps
import scala.language.reflectiveCalls

object Lifting {

  def liftFunctionExpr[T <: Type](p: Expr[DataType -> T]): DataExpr => Expr[T] = {
    p match {
      case l: LambdaExpr[T] =>
        (arg: DataExpr) =>  Expr.substitute(arg, `for` = l.param, in = l.body)
      case app: ApplyExpr[DataType -> T] =>
        val fun = liftFunctionExpr(app.fun)
        liftFunctionExpr(fun(app.arg))
      case app: NatDependentApplyExpr[DataType -> T] =>
        val fun = liftNatDependentFunctionExpr(app.fun)
        liftFunctionExpr(fun(app.arg))
      case app: TypeDependentApplyExpr[DataType -> T] =>
        val fun = liftTypeDependentFunctionExpr(app.fun)
        liftFunctionExpr(fun(app.arg))
      case IfThenElseExpr(_, _, _) =>
        throw new Exception("This should never happen")
    }
  }

  def liftNatDependentFunctionExpr[T <: Type](p: Expr[`(nat)->`[T]]): Nat => Expr[T] = {
    p match {
      case l: NatDependentLambdaExpr[T] =>
        (arg: Nat) => Type.substitute(arg, `for` = l.x, in = l.body)
      case app: ApplyExpr[`(nat)->`[T]] =>
        val fun = liftFunctionExpr(app.fun)
        liftNatDependentFunctionExpr(fun(app.arg))
      case app: NatDependentApplyExpr[`(nat)->`[T]] =>
        val fun = liftNatDependentFunctionExpr(app.fun)
        liftNatDependentFunctionExpr(fun(app.arg))
      case app: TypeDependentApplyExpr[`(nat)->`[T]] =>
        val fun = liftTypeDependentFunctionExpr(app.fun)
        liftNatDependentFunctionExpr(fun(app.arg))
      case IfThenElseExpr(_, _, _) =>
        throw new Exception("This should never happen")
    }
  }

  def liftTypeDependentFunctionExpr[T <: Type](p: Expr[`(dt)->`[T]]): DataType => Expr[T] = {
    p match {
      case l: TypeDependentLambdaExpr[T] =>
        (arg: DataType) => Type.substitute(arg, `for` = l.x, in = l.body)
      case app: ApplyExpr[`(dt)->`[T]] =>
        val fun = liftFunctionExpr(app.fun)
        liftTypeDependentFunctionExpr(fun(app.arg))
      case app: NatDependentApplyExpr[`(dt)->`[T]] =>
        val fun = liftNatDependentFunctionExpr(app.fun)
        liftTypeDependentFunctionExpr(fun(app.arg))
      case app: TypeDependentApplyExpr[`(dt)->`[T]] =>
        val fun = liftTypeDependentFunctionExpr(app.fun)
        liftTypeDependentFunctionExpr(fun(app.arg))
      case IfThenElseExpr(_, _, _) =>
        throw new Exception("This should never happen")
    }
  }

}
