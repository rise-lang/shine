package elevate

import lift.core._

package object core {
  type Strategy = Expr => Expr

  case class NotFound(s: Strategy) extends Exception

  def mayApply: Strategy => Expr => Option[Expr] =
    s => e => {
      try { Some(s(e)) }
      catch {
        case _: MatchError | NotFound(_) => None
      }
    }

  def mayApply2: Strategy => Expr => (Option[Expr], Strategy) =
    s => e => {
      try { (Some(s(e)), s) }
      catch {
        case NotFound(x) => (None, x)
      }
    }


  implicit class Then(f: Strategy) {
    def `;`(s: Strategy): Strategy = strategies.seq(f)(s)
  }

  implicit class LeftChoice(f: Strategy) {
    def <+(s: Strategy): Strategy = strategies.leftChoice(f)(s)
  }
}
