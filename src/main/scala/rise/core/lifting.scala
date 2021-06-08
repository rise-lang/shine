package rise.core

import rise.core.types._
import rise.core.DSL._

object lifting {
  sealed trait Result[+T] {
    val value: T

    def bind[R](rf: T => Result[R], ef: T => Result[R]): Result[R]

    def map[R](f: T => R): Result[R] =
      bind(v => Reducing(f(v)), v => Expanding(f(v)))

    def reducing: T
  }
  case class Reducing[+T](override val value: T) extends Result[T] {
    override def bind[R](rf: T => Result[R], ef: T => Result[R]): Result[R] =
      rf(value)

    override def reducing: T = value
  }
  case class Expanding[+T](override val value: T) extends Result[T] {
    override def bind[R](rf: T => Result[R], ef: T => Result[R]): Result[R] =
      ef(value)

    override def reducing: T =
      throw new Exception("lifting was not reducing")
  }

  // p : a -> b
  def liftFunExpr(p: Expr): Result[Expr => Expr] = {
    def chain(r: Result[Expr]): Result[Expr => Expr] =
      r.bind(liftFunExpr, f => Expanding((e: Expr) => app(f, e)))

    p match {
      case Lambda(x, body) =>
        Reducing((e: Expr) => substitute.exprInExpr(e, `for` = x, in = body))
      case App(f, e) => chain(liftFunExpr(f).map(lf => lf(e)))
      case DepApp(kind, f, x) => chain(liftDepFunExpr(kind, f).map(lf => lf(x)))
      case _ => chain(Expanding(p))
    }
  }

  def liftDepFunExpr[T, KI <: Kind.Identifier](kind: Kind[T, _, KI], p: Expr): Result[T => Expr] = {
    def chain(r: Result[Expr]): Result[T => Expr] =
      r.bind(liftDepFunExpr[T, KI](kind, _), f => Expanding((x: T) => depApp(kind, f, x)))

    p match {
      case DepLambda(kind, x, e) =>
        Reducing((a: T) => substitute.kindInExpr(kind, a, `for` = x, in = e))
      case App(f, e) => chain(liftFunExpr(f).map(lf => lf(e)))
      case DepApp(kind, f, x) =>  chain(liftDepFunExpr(kind, f).map(lf => lf(x)))
      case _ => chain(Expanding(p))
    }
  }

  def liftDependentFunctionType[T, I](kind: Kind[T, I, _], ty: Type): T => Type = {
    ty match {
      case DepFunType(kind, x, t) =>
        (a: T) => substitute.kindInType(kind, a, `for` = x, in = t)
      case _ => throw new Exception(s"did not expect $ty")
    }
  }
}
