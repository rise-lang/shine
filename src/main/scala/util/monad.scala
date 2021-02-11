package util

import scala.language.implicitConversions

object monad {
  trait Monad[M[_]] {
    def return_[T] : T => M[T]
    def bind[T,S] : M[T] => (T => M[S]) => M[S]
    def traverse[A] : Seq[M[A]] => M[Seq[A]] =
      _.foldRight(return_(Nil : Seq[A]))({case (mx, mxs) =>
        bind(mx)(x => bind(mxs)(xs => return_(x +: xs)))})
  }

  implicit def monadicSyntax[M[_], A](m: M[A])(implicit tc: Monad[M]) = new {
    def map[B](f: A => B): M[B] = tc.bind(m)(a => tc.return_(f(a)) )
    def flatMap[B](f: A => M[B]): M[B] = tc.bind(m)(f)
  }

  case class Pure[T](unwrap : T)
  implicit object PureMonad extends Monad[Pure] {
    override def return_[T] : T => Pure[T] = t => Pure(t)
    override def bind[T,S] : Pure[T] => (T => Pure[S]) => Pure[S] =
      v => f => v match { case Pure(v) => f(v) }
  }

  implicit object OptionMonad extends Monad[Option] {
    def return_[T]: T => Option[T] = Some(_)
    def bind[T, S]: Option[T] => (T => Option[S]) => Option[S] = v => v.flatMap
  }

}
