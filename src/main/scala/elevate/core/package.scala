package elevate

import _root_.lift.core._
import elevate.core.strategies.basic._

package object core {
  type StrategyT[T <: Program] = Program => RewriteResult[T]
  type Strategy = Program => RewriteResult[Expr]

  sealed trait RewriteResult[T <: Program] {
    def getProgramOrElse[T <: Program](e: T): T
    def get[T <: Program]: T

    def mapSuccess(f: Program => Program): RewriteResult[T]
    def flatMapSuccess(f: Program => RewriteResult[T]): RewriteResult[T]

    def mapFailure(f: Strategy => Strategy): RewriteResult[T]
    def flatMapFailure(f: Strategy => RewriteResult[T]): RewriteResult[T]
  }

  case class Success[T <: Program](e: Program) extends RewriteResult[T] {
    override def getProgramOrElse[U <: Program](u: U): U = e match {case y:U => y}
    override def get[U <: Program]: U = e match {case x:U => x}

    override def mapSuccess(f: Program => Program): RewriteResult[T] = Success(f(e))
    override def flatMapSuccess(f: Program => RewriteResult[T]): RewriteResult[T] = f(e)

    override def mapFailure(f: Strategy => Strategy): RewriteResult[T] = this
    override def flatMapFailure(f: Strategy => RewriteResult[T]): RewriteResult[T] = this
  }

  case class Failure[T <: Program](s: Strategy) extends RewriteResult[T] {
    override def getProgramOrElse[U <: Program](u: U): U = u
    override def get[U <: Program]: U = throw NotApplicable(s)

    override def mapSuccess(f: Program => Program): RewriteResult[T] = this
    override def flatMapSuccess(f: Program => RewriteResult[T]): RewriteResult[T] = this

    override def mapFailure(f: Strategy => Strategy): RewriteResult[T] = Failure(f(s))
    override def flatMapFailure(f: Strategy => RewriteResult[T]): RewriteResult[T] = f(s)
  }

  case class NotApplicable(s: Strategy) extends Exception

  implicit class Then(f: Strategy) {
    def `;`(s: Strategy): Strategy = seq(f)(s)
  }

  implicit class LeftChoice(f: Strategy) {
    def <+(s: Strategy): Strategy = leftChoice(f)(s)
  }
}
