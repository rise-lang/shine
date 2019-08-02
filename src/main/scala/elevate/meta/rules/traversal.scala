package elevate.meta.rules

import elevate.core.{Elevate, Failure, RewriteResult, Rule, Success}
import elevate.core.strategies.basic.seq
import elevate.lift.strategies.traversal.body

object traversal {
  case object bodyFission extends Rule[Elevate] {
    def apply(e: Elevate): RewriteResult[Elevate] = e match {
      case body(seq(f,s)) => Success(seq(body(f),body(s)))
      case x => Failure(bodyFission)
    }
  }

}
