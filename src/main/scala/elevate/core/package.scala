package elevate

import _root_.lift.core._
import elevate.core.strategies.basic._

package object core {
  type Strategy = Expr => RewriteResult

  implicit class Then(f: Strategy) {
    def `;`(s: Strategy): Strategy = seq(f)(s)
  }

  implicit class LeftChoice(f: Strategy) {
    def <+(s: Strategy): Strategy = leftChoice(f)(s)
  }
}
