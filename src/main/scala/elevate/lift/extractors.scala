package elevate.lift

import lift.core._
import lift.core.types._

object extractors {

  object :: {
    def unapply(e: Expr): Option[(Expr, Type)] = e match {
      case _ => Some((e, e.t))
    }
  }

}
