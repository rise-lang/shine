package elevate

import lift.core._

package object core {
  type Strategy = Expr => Expr

  sealed trait RewriteResult
  case class Success(e: Expr) extends RewriteResult
  case class Failure(s: Strategy) extends RewriteResult

  case class NotApplicable(s: Strategy) extends Exception

  def mayApply: Strategy => Expr => Option[Expr] =
    s => e => {
      try { Some(s(e)) }
      catch {
        case _: MatchError | NotApplicable(_) => None
      }
    }

  def mayApply2: Strategy => Expr => RewriteResult =
    s => e => {
      try { Success(s(e)) }
      catch {
        case NotApplicable(x) => Failure(x)
      }
    }


  implicit class Then(f: Strategy) {
    def `;`(s: Strategy): Strategy = strategies.seq(f)(s)
  }

  implicit class LeftChoice(f: Strategy) {
    def <+(s: Strategy): Strategy = strategies.leftChoice(f)(s)
  }
}
