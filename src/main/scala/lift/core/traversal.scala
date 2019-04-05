package lift.core

import lift.core.types._

object traversal {
  sealed abstract class Result[+T](val value: T) {
    def map[U](f: T => U): Result[U]
  }

  final case class Stop[+T](override val value: T) extends Result[T](value) {
    override def map[U](f: T => U): Result[U] = Stop(f(value))
  }

  final case class Continue[+T](override val value: T, v: Visitor) extends Result[T](value) {
    override def map[U](f: T => U): Result[U] = Continue(f(value), v)
  }

  class Visitor {
    def apply(e: Expr): Result[Expr] = Continue(e, this)

    def apply(ae: Nat): Nat = ae

    def apply(f: NatNatTypeFunction): NatNatTypeFunction = NatNatTypeFunction(f.x, apply(f.body))

    def apply[T <: Type](t: T): T = t
  }

  object DepthFirstLocalResult {
    def apply(expr: Expr, v: Visitor): Expr = {
      v(expr) match {
        case s: Stop[Expr] => s.value
        case c: Continue[Expr] =>
          val v = c.v
          c.value match {
            case i: Identifier => i
            case Lambda(x, e) =>
              Lambda(x, apply(e, v))
            case Apply(f, e) =>
              Apply(apply(f, v), apply(e, v))
            case NatLambda(n, e) =>
              NatLambda(n, apply(e, v))
            case NatApply(f, n) =>
              NatApply(apply(f, v), v(n))
            case TypeLambda(dt, e) =>
              TypeLambda(dt, apply(e, v))
            case TypeApply(f, dt) =>
              TypeApply(apply(f, v), v(dt))
            case l: Literal => l
            case Index(n, size) =>
              Index(v(n), v(size))
            case NatExpr(n) =>
              NatExpr(v(n))
            case IfThenElse(ce, te, ee) =>
              IfThenElse(apply(ce, v), apply(te, v), apply(ee, v))
            case TypedExpr(e, t) =>
              TypedExpr(apply(e, v), v(t))
            case p: Primitive => p
          }
      }
    }
  }

  object DepthFirstGlobalResult {
    def chain[A, B](a: Result[A], b: B,
                    visit_b: Visitor => Result[B]): Result[(A, B)] = {
      a match {
        case Stop(as) => Stop((as, b))
        case Continue(ac, vc) => visit_b(vc).map((ac, _))
      }
    }

    def chainE[A](a: Result[A], e: Expr) =
      chain(a, e, apply(e, _))

    def chainN[A](a: Result[A], n: Nat)=
      chain(a, n, v => Continue(v(n), v))

    def chainT[A, T <: Type](a: Result[A], t: T) =
      chain(a, t, v => Continue(v(t), v))

    def apply(expr: Expr, visit: Visitor): Result[Expr] = {
      visit(expr) match {
        case Stop(r) => Stop(r)
        case Continue(c, v) => c match {
          case i: Identifier => Continue(i, v)
          case Lambda(x, e) =>
            apply(e, v).map(Lambda(x, _))
          case Apply(f, e) =>
            chainE(apply(f, v), e).map(r => Apply(r._1, r._2))
          case NatLambda(n, e) =>
            apply(e, v).map(NatLambda(n, _))
          case NatApply(f, n) =>
            chainN(apply(f, v), n).map(r => NatApply(r._1, r._2))
          case TypeLambda(dt, e) =>
            apply(e, v).map(TypeLambda(dt, _))
          case TypeApply(f, dt) =>
            chainT(apply(f, v), dt).map(r => TypeApply(r._1, r._2))
          case l: Literal => Continue(l, v)
          case Index(n, size) =>
            Continue(Index(v(n), v(size)), v)
          case NatExpr(n) =>
            Continue(NatExpr(v(n)), v)
          case IfThenElse(ce, te, ee) => ???
          case TypedExpr(e, t) =>
            chainT(apply(e, v), t).map(r => TypedExpr(r._1, r._2))
          case p: Primitive => Continue(p, v)
        }
      }
    }
  }
}