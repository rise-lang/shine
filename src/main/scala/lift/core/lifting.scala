package lift.core

import lift.core.types.DataType

object lifting {
  sealed trait Result[+T] {
    val value: T

    def bind[R](rf: T => Result[R],
                ef: T => Result[R]): Result[R]

    def map[R](f: T => R): Result[R] =
      bind(v => Reduced(f(v)), v => Extended(f(v)))
  }
  case class Reduced[+T](override val value: T) extends Result[T] {
    override def bind[R](rf: T => Result[R],
                         ef: T => Result[R]): Result[R] = rf(value)
  }
  case class Extended[+T](override val value: T) extends Result[T] {
    override def bind[R](rf: T => Result[R],
                         ef: T => Result[R]): Result[R] = ef(value)
  }

  def liftFunctionExpr(p: Expr): Result[Expr => Expr] = {
    def chain(r: Result[Expr]): Result[Expr => Expr] =
      r.bind(liftFunctionExpr,
        f => Extended((e: Expr) => Apply(f, e)))

    p match {
      case Lambda(x, body) =>
        Reduced((e: Expr) => substitute(e, `for` = x, in = body))
      case Apply(f, e) =>
        chain(liftFunctionExpr(f).map(lf => lf(e)))
      case NatDepApply(f, n) =>
        chain(liftNatDependentFunctionExpr(f).map(lf => lf(n)))
      case TypeDepApply(f, t) =>
        chain(liftTypeDependentFunctionExpr(f).map(lf => lf(t)))
      case _ => chain(Extended(p))
    }
  }

  def liftNatDependentFunctionExpr(p: Expr): Result[Nat => Expr] = {
    def chain(r: Result[Expr]): Result[Nat => Expr] =
      r.bind(liftNatDependentFunctionExpr,
        f => Extended((n: Nat) => NatDepApply(f, n)))

    p match {
      case NatDepLambda(x, e) =>
        Reduced((n: Nat) => substitute(n, `for` = x, in = e))
      case Apply(f, e) =>
        chain(liftFunctionExpr(f).map(lf => lf(e)))
      case NatDepApply(f, n) =>
        chain(liftNatDependentFunctionExpr(f).map(lf => lf(n)))
      case TypeDepApply(f, t) =>
        chain(liftTypeDependentFunctionExpr(f).map(lf => lf(t)))
      case _ => chain(Extended(p))
    }
  }

  def liftTypeDependentFunctionExpr(p: Expr): Result[DataType => Expr] = {
    def chain(r: Result[Expr]): Result[DataType => Expr] =
      r.bind(liftTypeDependentFunctionExpr,
        f => Extended((dt: DataType) => TypeDepApply(f, dt)))

    p match {
      case TypeDepLambda(x, e) =>
        Reduced((dt: DataType) => substitute(dt, `for` = x, in = e))
      case Apply(f, e) =>
        chain(liftFunctionExpr(f).map(lf => lf(e)))
      case NatDepApply(f, n) =>
        chain(liftNatDependentFunctionExpr(f).map(lf => lf(n)))
      case TypeDepApply(f, t) =>
        chain(liftTypeDependentFunctionExpr(f).map(lf => lf(t)))
      case _ => chain(Extended(p))
    }
  }

}
