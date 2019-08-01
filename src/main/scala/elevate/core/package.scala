package elevate

import _root_.lift.core._
import elevate.core.strategies.basic._

package object core {
  //type StrategyT[T <: Program] = Program => RewriteResult[T]
  //type Strategy = Program => RewriteResult[Expr]

  trait StrategyT[T <: Program] extends Program {
    def apply(p: T): RewriteResult[T]
  }

  // the typical elevate strategy to rewrite lift programs
  trait Strategy extends StrategyT[Expr]

  sealed trait RewriteResult[T <: Program] {
    def getProgramOrElse[T <: Program](e: T): T
    def get[T <: Program]: T

    def mapSuccess(f: Program => Program): RewriteResult[T]
    def flatMapSuccess(f: Program => RewriteResult[T]): RewriteResult[T]

    def mapFailure(f: StrategyT[T] => StrategyT[T]): RewriteResult[T]
    def flatMapFailure(f: StrategyT[T] => RewriteResult[T]): RewriteResult[T]
  }

  case class Success[T <: Program](e: Program) extends RewriteResult[T] {
    override def getProgramOrElse[U <: Program](u: U): U = e match {case y:U => y}
    override def get[U <: Program]: U = e match {case x:U => x}

    override def mapSuccess(f: Program => Program): RewriteResult[T] = Success(f(e))
    override def flatMapSuccess(f: Program => RewriteResult[T]): RewriteResult[T] = f(e)

    override def mapFailure(f: StrategyT[T] => StrategyT[T]): RewriteResult[T] = this
    override def flatMapFailure(f: StrategyT[T] => RewriteResult[T]): RewriteResult[T] = this
  }

  case class Failure[T <: Program](s: StrategyT[T]) extends RewriteResult[T] {
    override def getProgramOrElse[U <: Program](u: U): U = u
    override def get[U <: Program]: U = throw NotApplicable(s)

    override def mapSuccess(f: Program => Program): RewriteResult[T] = this
    override def flatMapSuccess(f: Program => RewriteResult[T]): RewriteResult[T] = this

    override def mapFailure(f: StrategyT[T] => StrategyT[T]): RewriteResult[T] = Failure(f(s))
    override def flatMapFailure(f: StrategyT[T] => RewriteResult[T]): RewriteResult[T] = f(s)
  }

  case class NotApplicable[T <: Program](s: StrategyT[T]) extends Exception

  implicit class Then[T <: Program](f: StrategyT[T]) {
    def `;`(s: StrategyT[T]): StrategyT[T] = seq[T](f,s)
  }

  implicit class LeftChoice[T <: Program](f: StrategyT[T]) {
    def <+(s: StrategyT[T]): StrategyT[T] = leftChoice(f,s)
  }
}
