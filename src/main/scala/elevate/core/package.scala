package elevate

import elevate.core.strategies.basic._
import scala.language.implicitConversions

package object core {

  type Strategy[P] = P => RewriteResult[P]
  type MetaStrategy[P] = Strategy[Strategy[P]]

  implicit def rewriteResultToP[P](r: RewriteResult[P]): P = r.get

  implicit class Then[P](f: Strategy[P]) {
    def `;`(s: Strategy[P]): Strategy[P] = seq[P](f,s)
  }

  implicit class LeftChoice[P](f: Strategy[P]) {
    def <+(s: Strategy[P]): Strategy[P] = leftChoice(f,s)
  }
}
