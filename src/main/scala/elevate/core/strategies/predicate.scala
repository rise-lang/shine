package elevate.core.strategies

import elevate.core.{Failure, Lift, RewriteResult, Strategy, Success}
import elevate.core.strategies.traversal.oncetd

import scala.language.implicitConversions

object predicate {

  implicit def rewriteResultToBoolean[P](r: RewriteResult[P]): Boolean = r match {
    case Failure(_) => false
    case Success(_) => true
  }

  case class isEqualTo[P](x: P) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] =
      if (e == x) Success(e) else Failure(isEqualTo(x))
  }

  // todo generalize once "oncetd" is generic too
  case class contains(x: Lift) extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = oncetd(isEqualTo(x))(e)
  }

}
