package elevate

import lift.core._

package object core {
  type Strategy = Expr => Expr

  case object NotFound extends Exception

  def mayApply: Strategy => Expr => Option[Expr] =
    s => e => {
      try { Some(s(e)) }
      catch {
        case _: MatchError | NotFound => None
      }
    }

  implicit class Then(f: Strategy) {
    def `;`(s: Strategy): Strategy = strategies.seq(f)(s)
  }

  implicit class LeftChoice(f: Strategy) {
    def <+(s: Strategy): Strategy = strategies.leftChoice(f)(s)
  }
}
