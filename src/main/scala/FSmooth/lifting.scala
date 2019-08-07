package FSmooth

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

  def liftFunExpr(p: Expr): Result[Seq[Expr] => Expr] = {
    def chain(r: Result[Expr]): Result[Seq[Expr] => Expr] =
      r.bind(liftFunExpr,
        f => Expanding((es: Seq[Expr]) => Application(f, es)))

    p match {
      case Abstraction(xs, body)  =>
        Reducing((es: Seq[Expr])  => substitute(es, `for` = xs, in = body))
      case Application(f, es)     => chain(liftFunExpr(f).map(lf => lf(es)))
      case _                      => chain(Expanding(p))
    }
  }
}

