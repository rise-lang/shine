package elevate

import _root_.rise.core._
import _root_.rise.core.types.Type

package object rise {
  type Rise = Expr

  // type-extractor
  object :: {
    def unapply(e: Expr): Option[(Expr, Type)] = Some((e, e.t))
  }
}
