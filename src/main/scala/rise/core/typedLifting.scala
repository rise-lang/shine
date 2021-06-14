package rise.core

import rise.core.types._
import rise.core.lifting.{Expanding, Reducing, Result}

object typedLifting {
  // p : a -> b
  def liftFunExpr(p: Expr): Result[Expr => Expr] = {
    def chain(r: Result[Expr]): Result[Expr => Expr] =
      r.bind(
        liftFunExpr,
        f =>
          Expanding((e: Expr) =>
            App(f, e)(f.t match {
              case FunType(_, outT) => outT
              case _ => throw TypeException(s"$f cannot be lifted")
            })
          )
      )

    p match {
      case Lambda(x, body) =>
        Reducing((e: Expr) => substitute.exprInExpr(e, `for` = x, in = body))
      case App(f, e) => chain(liftFunExpr(f).map(lf => lf(e)))
      case DepApp(kind, f, x) => chain(liftDepFunExpr(kind, f).map(lf => lf(x)))
      case _ => chain(Expanding(p))
    }
  }

  def liftDepFunExpr[T,KI <: Kind.Identifier](kind: Kind[T, _, KI], p: Expr): Result[T => Expr] = {
    def chain(r: Result[Expr]): Result[T => Expr] =
      r.bind(
        liftDepFunExpr(kind, _),
        f =>
          Expanding((x: T) =>
            DepApp(kind, f, x)(f.t match {
              case DepFunType(_, _, _) => lifting.liftDependentFunctionType(kind, f.t)(x)
              case _ => throw TypeException(s"$f cannot be lifted")
            })
          )
      )

    p match {
      case DepLambda(kind, x, e) =>
        Reducing((a: T) => substitute.kindInExpr(kind, a, `for` = x, in = e))
      case App(f, e) => chain(liftFunExpr(f).map(lf => lf(e)))
      case DepApp(kind, f, x) => chain(liftDepFunExpr(kind, f).map(lf => lf(x)))
      case _ => chain(Expanding(p))
    }
  }
}
