package elevate.rise

import lift.core._
import lift.core.types._

object extractors {

  object :: {
    def unapply(e: Expr): Option[(Expr, Type)] = Some((e, e.t))
  }

}
