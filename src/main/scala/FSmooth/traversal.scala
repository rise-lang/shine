package FSmooth

object traversal {
  sealed abstract class Result[+T](val value: T) {
    def map[U](f: T => U): Result[U]
    def mapVisitor(f: Visitor => Visitor): Result[T]
  }

  final case class Stop[+T](override val value: T) extends Result[T](value) {
    override def map[U](f: T => U): Result[U] = Stop(f(value))
    def mapVisitor(f: Visitor => Visitor): Result[T] = this
  }

  final case class Continue[+T](override val value: T, v: Visitor) extends Result[T](value) {
    override def map[U](f: T => U): Result[U] = Continue(f(value), v)
    def mapVisitor(f: Visitor => Visitor): Result[T] = Continue(value, f(v))
  }

  class Visitor {
    def apply(e: Expr): Result[Expr] = Continue(e, this)
  }

  object DepthFirstLocalResult {
    def apply(expr: Expr, v: Visitor): Expr = {
      v.apply(expr) match {
        case s: Stop[Expr] => s.value
        case c: Continue[Expr] =>
          val v = c.v
          c.value match {
            case Abstraction(xs, e) => Abstraction(xs, apply(e, v))
            case Application(f, es) => Application(apply(f, v), es.map(apply(_, v)))
            case Let(x, init, e) => Let(x, apply(init, v), apply(e, v))
            case Conditional(c, t, e) => Conditional(apply(c, v), apply(t, v), apply(e, v))
            case Identifier(_) | ScalarValue(_) | IndexValue(_) |
                 CardinalityValue(_) | _: Constants
              => c.value
          }
      }
    }
  }
}
