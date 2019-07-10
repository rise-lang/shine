package elevate

import lift.core._

package object core {
  type Strategy = Expr => RewriteResult

  sealed trait RewriteResult {
    def getExprOrElse(e: Expr): Expr
    def get: Expr
  }

  case class Success(e: Expr) extends RewriteResult {
    override def getExprOrElse(expr: Expr): Expr = e
    override def get: Expr = e
  }

  case class Failure(s: Strategy) extends RewriteResult {
    override def getExprOrElse(e: Expr): Expr = e
    override def get: Expr = throw NotApplicable(s)
  }

  case class NotApplicable(s: Strategy) extends Exception

  implicit class Then(f: Strategy) {
    def `;`(s: Strategy): Strategy = strategies.seq(f)(s)
  }

  implicit class LeftChoice(f: Strategy) {
    def <+(s: Strategy): Strategy = strategies.leftChoice(f)(s)
  }
}
