package elevate

import _root_.lift.core._
import elevate.core.strategies.basic._

package object core {

  type Strategy[P] = P => RewriteResult[P]

  type Meta = Strategy[Elevate] // Meta = Strategies for Elevate
  type Elevate = Strategy[Lift] // Elevate = Strategies for Lift
  type Lift = Expr

  implicit class Then[P](f: Strategy[P]) {
    def `;`(s: Strategy[P]): Strategy[P] = seq[P](f,s)
  }

  implicit class LeftChoice[P](f: Strategy[P]) {
    def <+(s: Strategy[P]): Strategy[P] = leftChoice(f,s)
  }
}
