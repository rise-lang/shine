package rise.core

import rise.core.types._
import rise.core.DSL._

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
  def liftFunExpr(p: Expr): Result[Expr => Expr] = {
    def chain(r: Result[Expr]): Result[Expr => Expr] =
      r.bind(liftFunExpr,
        f => Expanding((e: Expr) => app(f, e)))

    p match {
      case Lambda(x, body)    => Reducing((e: Expr) => substitute.exprInExpr(e, `for` = x, in = body))
      case App(f, e)        => chain(liftFunExpr(f).map(lf => lf(e)))
      case DepApp(f, x)     => x match {
        case t: DataType      => chain(liftDepFunExpr[DataKind](f).map(lf => lf(t)))
        case n: Nat           => chain(liftDepFunExpr[NatKind](f).map(lf => lf(n)))
        case a: AddressSpace  => chain(liftDepFunExpr[AddressSpaceKind](f).map(lf => lf(a)))
        case n2n: NatToNat    => chain(liftDepFunExpr[NatToNatKind](f).map(lf => lf(n2n)))
        case n2d: NatToData   => chain(liftDepFunExpr[NatToDataKind](f).map(lf => lf(n2d)))
      }
      case _                  => chain(Expanding(p))
    }
  }

  def liftDepFunExpr[K <: Kind](p: Expr): Result[K#T => Expr] = {
    def chain(r: Result[Expr]): Result[K#T => Expr] =
      r.bind(liftDepFunExpr,
        f => Expanding((x: K#T) => depApp[K](f, x)))

    p match {
      case DepLambda(x, e)    => Reducing((a: K#T) => substitute.kindInExpr(a, `for` = x, in = e))
      case App(f, e)        => chain(liftFunExpr(f).map(lf => lf(e)))
      case DepApp(f, x)     => x match {
        case t: DataType      => chain(liftDepFunExpr[DataKind](f).map(lf => lf(t)))
        case n: Nat           => chain(liftDepFunExpr[NatKind](f).map(lf => lf(n)))
        case a: AddressSpace  => chain(liftDepFunExpr[AddressSpaceKind](f).map(lf => lf(a)))
        case n2n: NatToNat    => chain(liftDepFunExpr[NatToNatKind](f).map(lf => lf(n2n)))
        case n2d: NatToData   => chain(liftDepFunExpr[NatToDataKind](f).map(lf => lf(n2d)))
      }
      case _                  => chain(Expanding(p))
    }
  }

  def liftDependentFunctionType[K <: Kind](ty: Type): K#T => Type = {
    ty match {
      case DepFunType(x, t) => (a: K#T) => substitute.kindInType(a, `for`=x, in=t)
      case _ => throw new Exception(s"did not expect $ty")
    }
  }
}
