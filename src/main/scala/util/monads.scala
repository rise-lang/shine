package util

import scala.collection.immutable.HashSet
import scala.language.implicitConversions

object monads {
  trait Monad[M[_]] {
    def return_[T] : T => M[T]
    def bind[T,S] : M[T] => (T => M[S]) => M[S]
    def traverse[A] : Seq[M[A]] => M[Seq[A]] =
      _.foldRight(return_(Nil : Seq[A]))({case (mx, mxs) =>
        bind(mx)(x => bind(mxs)(xs => return_(x +: xs)))})
  }

  implicit def monadicSyntax[M[_], A](m: M[A])(implicit tc: Monad[M]): MonadicSyntax[M, A] =
    new MonadicSyntax[M, A](m, tc)

  class MonadicSyntax[M[_], A](m: M[A], tc: Monad[M]) {
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
    override def return_[T]: T => Option[T] = Some(_)
    override def bind[T, S]: Option[T] => (T => Option[S]) => Option[S] = v => v.flatMap
  }

  trait Monoid[T] {
    def empty : T
    def append : T => T => T
  }

  implicit val OrMonoid : Monoid[Boolean] = new Monoid[Boolean] {
    def empty : Boolean = false
    def append : Boolean => Boolean => Boolean = x => y => x || y
  }

  implicit val AddMonoid : Monoid[Int] = new Monoid[Int] {
    def empty : Int = 0
    def append : Int => Int => Int = x => y => x + y
  }

  implicit def SeqMonoid[T] : Monoid[Seq[T]] = new Monoid[Seq[T]] {
    def empty : Seq[T] = Seq()
    def append : Seq[T] => Seq[T] => Seq[T] = x => y => x ++ y
  }

  implicit def SetMonoid[T] : Monoid[Set[T]] = new Monoid[Set[T]] {
    def empty : Set[T] = HashSet()
    def append : Set[T] => Set[T] => Set[T] = x => y => x ++ y
  }

  implicit def MapMonoid[K,V] : Monoid[Map[K,V]] = new Monoid[Map[K,V]] {
    def empty : Map[K,V] = Map()
    def append : Map[K,V] => Map[K,V] => Map[K,V] = x => y => x ++ y
  }

  implicit def PairMonoid[F,S](fst : Monoid[F], snd : Monoid[S]) : Monoid[Tuple2[F,S]] = new Monoid[Tuple2[F,S]] {
    override def empty : Tuple2[F,S] = (fst.empty, snd.empty)
    override def append : Tuple2[F,S] => Tuple2[F,S] => Tuple2[F,S] = {
      case (f1, s1) => { case (f2, s2) => (fst.append(f1)(f2), snd.append(s1)(s2)) }
    }
  }

  trait InMonad[M[_]] { trait SetFst[F] { type Type[S] = M[Tuple2[F, S]] } }
  trait PairMonoidMonad[F, M[_]] extends Monad[InMonad[M]#SetFst[F]#Type] {
    type Pair[T] = InMonad[M]#SetFst[F]#Type[T]
    implicit val monoid : Monoid[F]
    implicit val monad : Monad[M]
    override def return_[T]: T => Pair[T] = t => monad.return_((monoid.empty, t))
    override def bind[T, S]: Pair[T] => (T => Pair[S]) => Pair[S] = t => f =>
      monad.bind(t) { case (rs1, t) => monad.bind(f(t)) { case (rs2, s) =>
        monad.return_((monoid.append(rs1)(rs2), s))}}
  }
}
