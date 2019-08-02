package elevate

import _root_.lift.core._
import elevate.core.strategies.basic._

package object core {

  // Rule is just a different name for Strategy
  type Rule[T <: Program] = Strategy[T]
  trait Strategy[T <: Program] extends Program { def apply(p: T): RewriteResult[T] }

  type Meta = Strategy[Elevate] // Meta = Strategies for Elevate
  type Elevate = Strategy[Lift] // Elevate = Strategies for Lift
  type Lift = Expr

  sealed trait RewriteResult[T <: Program] {
    def getProgramOrElse[T <: Program](e: T): T
    def get[T <: Program]: T

    def mapSuccess(f: Program => Program): RewriteResult[T]
    def flatMapSuccess(f: Program => RewriteResult[T]): RewriteResult[T]

    def mapFailure(f: Strategy[T] => Strategy[T]): RewriteResult[T]
    def flatMapFailure(f: Strategy[T] => RewriteResult[T]): RewriteResult[T]
  }

  case class Success[T <: Program](e: Program) extends RewriteResult[T] {
    override def getProgramOrElse[U <: Program](u: U): U = e match {case y:U => y}
    override def get[U <: Program]: U = e match {case x:U => x}

    override def mapSuccess(f: Program => Program): RewriteResult[T] = Success(f(e))
    override def flatMapSuccess(f: Program => RewriteResult[T]): RewriteResult[T] = f(e)

    override def mapFailure(f: Strategy[T] => Strategy[T]): RewriteResult[T] = this
    override def flatMapFailure(f: Strategy[T] => RewriteResult[T]): RewriteResult[T] = this
  }

  case class Failure[T <: Program](s: Strategy[T]) extends RewriteResult[T] {
    override def getProgramOrElse[U <: Program](u: U): U = u
    override def get[U <: Program]: U = throw NotApplicable(s)

    override def mapSuccess(f: Program => Program): RewriteResult[T] = this
    override def flatMapSuccess(f: Program => RewriteResult[T]): RewriteResult[T] = this

    override def mapFailure(f: Strategy[T] => Strategy[T]): RewriteResult[T] = Failure(f(s))
    override def flatMapFailure(f: Strategy[T] => RewriteResult[T]): RewriteResult[T] = f(s)
  }

  case class NotApplicable[T <: Program](s: Strategy[T]) extends Exception

  implicit class Then[T <: Program](f: Strategy[T]) {
    def `;`(s: Strategy[T]): Strategy[T] = seq[T](f,s)
  }

  implicit class LeftChoice[T <: Program](f: Strategy[T]) {
    def <+(s: Strategy[T]): Strategy[T] = leftChoice(f,s)
  }
}
