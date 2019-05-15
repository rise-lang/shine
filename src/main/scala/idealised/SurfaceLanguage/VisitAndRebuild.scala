package idealised.SurfaceLanguage

import idealised.DPIA.NatNatLambda
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.Semantics._

object VisitAndRebuild {
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
    def apply(f:NatNatLambda):NatNatLambda = NatNatLambda(f.x, apply(f.body))
    def apply[T <: Type](t: T): T = t
  }

  def apply(e: Expr, v: Visitor): Expr = {
    v(e) match {
      case s: Stop[Expr] => s.value
      case c: Continue[Expr] =>
        val v = c.v
        c.value.rebuild(c.value.children.map(applyAny(v)))
    }
  }

  def applyAny(v: Visitor): Any => Any = {
    case e: Expr => apply(e, v)
    case n: Nat => v(n)
    case f: NatNatLambda => v(f)
    case t: Type => v(t)
    case s: Seq[_] => s.map(applyAny(v))
    case o: Option[_] => o.map(applyAny(v))
    case otherwise => println(s"$otherwise"); ???
  }

  object DFS {
    def apply(e: Expr, v: Visitor): Result[Expr] = {
      v(e) match {
        case s: Stop[Expr] => s
        case c: Continue[Expr] =>
          val children = applySeq(c.v)(c.value.children)
          children.map(c.value.rebuild)
      }
    }

    def applySeq(v: Visitor): Seq[Any] => Result[Seq[Any]] = {
      case Nil => Continue(Nil, v)
      case x :: r => applyAny(v)(x) match {
        case s: Stop[Any] => Stop(s.value +: r)
        case c: Continue[Any] => applySeq(c.v)(r).map(r => c.value +: r)
      }
    }

    def applyAny(v: Visitor): Any => Result[Any] = {
      case e: Expr => apply(e, v)
      case n: Nat => Continue(v(n), v)
      case f: NatNatLambda => Continue(v(f), v)
      case t: Type => Continue(v(t), v)
      case s: Seq[_] => applySeq(v)(s)
      case None => Continue(None, v)
      case Some(x) => applyAny(v)(x).map(Some(_))
      case otherwise => println(s"$otherwise"); ???
    }
  }
}

