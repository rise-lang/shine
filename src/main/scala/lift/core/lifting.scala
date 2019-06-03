package lift.core

import lift.core.types._

object lifting {
  sealed trait Result[+T] {
    val value: T

    def bind[R](rf: T => Result[R],
                ef: T => Result[R]): Result[R]

    def map[R](f: T => R): Result[R] =
      bind(v => Reducing(f(v)), v => Expanding(f(v)))

    def reducing: T
  }
  case class Reducing[+T](override val value: T) extends Result[T] {
    override def bind[R](rf: T => Result[R],
                         ef: T => Result[R]): Result[R] = rf(value)

    override def reducing: T = value
  }
  case class Expanding[+T](override val value: T) extends Result[T] {
    override def bind[R](rf: T => Result[R],
                         ef: T => Result[R]): Result[R] = ef(value)

    override def reducing: T =
      throw new Exception("lifting was not reducing")
  }

  // p : a -> b
  def liftFunctionExpr(p: Expr): Result[Expr => Expr] = {
    def chain(r: Result[Expr]): Result[Expr => Expr] =
      r.bind(liftFunctionExpr,
        f => Expanding((e: Expr) => Apply(f, e)))

    p match {
      case Lambda(x, body) =>
        Reducing((e: Expr) => substitute(e, `for` = x, in = body))
      case Apply(f, e) =>
        chain(liftFunctionExpr(f).map(lf => lf(e)))
      case NatDepApply(f, n) =>
        chain(liftNatDependentFunctionExpr(f).map(lf => lf(n)))
      case TypeDepApply(f, t) =>
        chain(liftTypeDependentFunctionExpr(f).map(lf => lf(t)))
      case _ => chain(Expanding(p))
    }
  }

  def liftNatDependentFunctionExpr(p: Expr): Result[Nat => Expr] = {
    def chain(r: Result[Expr]): Result[Nat => Expr] =
      r.bind(liftNatDependentFunctionExpr,
        f => Expanding((n: Nat) => NatDepApply(f, n)))

    p match {
      case NatDepLambda(x, e) =>
        Reducing((n: Nat) => substitute(n, `for` = x, in = e))
      case Apply(f, e) =>
        chain(liftFunctionExpr(f).map(lf => lf(e)))
      case NatDepApply(f, n) =>
        chain(liftNatDependentFunctionExpr(f).map(lf => lf(n)))
      case TypeDepApply(f, t) =>
        chain(liftTypeDependentFunctionExpr(f).map(lf => lf(t)))
      case _ => chain(Expanding(p))
    }
  }

  def liftTypeDependentFunctionExpr(p: Expr): Result[DataType => Expr] = {
    def chain(r: Result[Expr]): Result[DataType => Expr] =
      r.bind(liftTypeDependentFunctionExpr,
        f => Expanding((dt: DataType) => TypeDepApply(f, dt)))

    p match {
      case TypeDepLambda(x, e) =>
        Reducing((dt: DataType) => substitute(dt, `for` = x, in = e))
      case Apply(f, e) =>
        chain(liftFunctionExpr(f).map(lf => lf(e)))
      case NatDepApply(f, n) =>
        chain(liftNatDependentFunctionExpr(f).map(lf => lf(n)))
      case TypeDepApply(f, t) =>
        chain(liftTypeDependentFunctionExpr(f).map(lf => lf(t)))
      case _ => chain(Expanding(p))
    }
  }

  def liftNatDependentFunctionType(ty: Type): Nat => Type = {
    ty match {
      case NatDependentFunctionType(x, t) =>
        (n: Nat) => substitute(n, `for`=x, in=t)
      case _ => ???
    }
  }

  def liftTypeDependentFunctionType(ty: Type): DataType => Type = {
    ty match {
      case TypeDependentFunctionType(x, t) =>
        (dt: DataType) => substitute(dt, `for`=x, in=t)
      case _ => ???
    }
  }
}
