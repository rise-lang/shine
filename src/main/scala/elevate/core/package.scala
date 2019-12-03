package elevate

import elevate.core.strategies.basic._

package object core {

  type Strategy[P] = P => RewriteResult[P]
  type MetaStrategy[P] = Strategy[Strategy[P]]

  implicit class Then[P](f: Strategy[P]) {
    def `;`(s: Strategy[P]): Strategy[P] = seq[P](f,s)
  }

  implicit class LeftChoice[P](f: Strategy[P]) {
    def <+(s: Strategy[P]): Strategy[P] = leftChoice(f,s)
  }
}
