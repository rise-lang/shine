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

  case class contains[P: Traversable](x: P) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = oncetd(isEqualTo(x)).apply(p)
  }

}
