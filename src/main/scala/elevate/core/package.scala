package elevate

import lift.core._
import elevate.core.strategies.basic._

package object core {
  type Strategy = Expr => RewriteResult

  sealed trait RewriteResult {
    def getExprOrElse(e: Expr): Expr
    def get: Expr

    def mapSuccess(f: Expr => Expr): RewriteResult
    def flatMapSuccess(f: Expr => RewriteResult): RewriteResult

    def mapFailure(f: Strategy => Strategy): RewriteResult
    def flatMapFailure(f: Strategy => RewriteResult): RewriteResult
  }

  case class Success(e: Expr) extends RewriteResult {
    override def getExprOrElse(expr: Expr): Expr = e
    override def get: Expr = e

    override def mapSuccess(f: Expr => Expr): RewriteResult = Success(f(e))
    override def flatMapSuccess(f: Expr => RewriteResult): RewriteResult = f(e)

    override def mapFailure(f: Strategy => Strategy): RewriteResult = this
    override def flatMapFailure(f: Strategy => RewriteResult): RewriteResult = this
  }

  case class Failure(s: Strategy) extends RewriteResult {
    override def getExprOrElse(e: Expr): Expr = e
    override def get: Expr = throw NotApplicable(s)

    override def mapSuccess(f: Expr => Expr): RewriteResult = this
    override def flatMapSuccess(f: Expr => RewriteResult): RewriteResult = this

    override def mapFailure(f: Strategy => Strategy): RewriteResult = Failure(f(s))
    override def flatMapFailure(f: Strategy => RewriteResult): RewriteResult = f(s)
  }

  case class NotApplicable(s: Strategy) extends Exception

  implicit class Then(f: Strategy) {
    def `;`(s: Strategy): Strategy = seq(f)(s)
  }

  implicit class LeftChoice(f: Strategy) {
    def <+(s: Strategy): Strategy = leftChoice(f)(s)
  }
}
