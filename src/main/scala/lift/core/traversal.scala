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

  object DepthFirstLocalStop {
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

  object DepthFirstGlobalStop {
    def apply(expr: Expr, v: Visitor): (Boolean, Expr) = {
      var stopped = false
      def wrap(v: Visitor): Visitor = new Visitor {
        override def apply(e: Expr): Result[Expr] = {
          if (stopped) {
            Stop(e)
          } else {
            v(e) match {
              case s: Stop[Expr] => stopped = true; s
              case c: Continue[Expr] => Continue(c.value, wrap(c.v))
            }
          }
        }
        override def apply(ae: Nat): Nat = v(ae)
        override def apply(f: NatNatTypeFunction): NatNatTypeFunction = v(f)
        override def apply[T <: Type](t: T): T = v(t)
      }

      val result = DepthFirstLocalStop(expr, wrap(v))
      (stopped, result)
    }
  }
}