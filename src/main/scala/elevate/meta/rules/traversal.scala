package elevate.meta.rules

import elevate.core.{Elevate, Failure, Lift, RewriteResult, Strategy, Success}
import elevate.core.strategies.basic.seq
import elevate.lift.strategies.traversal._

object traversal {
  case object bodyFission extends Strategy[Elevate] {
    def apply(e: Elevate): RewriteResult[Elevate] = e match {
      case body(seq(f,s)) => Success(seq(body(f),body(s)))
      case _ => Failure(bodyFission)
    }
  }


}
