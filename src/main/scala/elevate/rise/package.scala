package elevate

import lift.core._
import lift.core.types.Type

package object rise {
  type Rise = Expr

  // type-extractor
  object :: {
    def unapply(e: Expr): Option[(Expr, Type)] = Some((e, e.t))
  }
}
