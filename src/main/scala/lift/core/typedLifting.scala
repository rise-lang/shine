package lift.core

import lift.core.TypedDSL._
import lift.core.types._
import lift.core.lifting.{Result, Reducing, Expanding}

object typedLifting {
  // p : a -> b
  def liftFunExpr(p: Expr): Result[Expr => Expr] = {
    def chain(r: Result[Expr]): Result[Expr => Expr] =
      r.bind(liftFunExpr,
        f => Expanding((e: Expr) => app(f, e).matches(f.t match {
          case FunType(_, outT) => outT
          case _ => throw TypeException(s"$f cannot be lifted")
        })))

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
        f => Expanding((x: K#T) => depApp(f, x).matches(f.t match {
          case DepFunType(_, t) => t
          case _ => throw TypeException(s"$f cannot be lifted")
        })))

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
}
