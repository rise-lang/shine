package elevate.core.strategies

import elevate.core._
import elevate.core.strategies.traversal.oncetd

import scala.language.implicitConversions

object predicate {

  implicit def rewriteResultToBoolean[P](r: RewriteResult[P]): Boolean = r match {
    case Failure(_) => false
    case Success(_) => true
  }

  case class not[P](s: Strategy[P]) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = s(e) match {
      case Success(_) => Failure(not(s))
      case Failure(_) => Success(e)
    }
    override def toString = s"not($s)"
  }

  case class isEqualTo[P](x: P) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] =
      if (p == x) Success(p) else Failure(isEqualTo(x))
    override def toString = s"isEqualTo($x)"
  }

  case class contains[P: Traversable](x: P) extends Strategy[P] {
    def apply(p: P): RewriteResult[P] = oncetd(isEqualTo(x)).apply(p)
    override def toString = s"contains($x)"
  }
}
