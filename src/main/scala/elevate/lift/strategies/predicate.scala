package elevate.lift.strategies

import elevate.core.strategies.traversal.oncetd
import elevate.core.{Failure, RewriteResult, Strategy, Success}
import lift.core.{Expr, Identifier, Lambda}

import scala.language.implicitConversions

object predicate {

  implicit def rewriteResultToBoolean[P](r: RewriteResult[P]): Boolean = r match {
    case Failure(_) => false
    case Success(_) => true
  }

  /*
  def isLambda: Strategy = {
    case l:Lambda => Success(l)
    case _ => Failure(isLambda)
  }
   */

  case object isIdentifier extends Strategy[Expr] {
    def apply(e: Expr): RewriteResult[Expr] = e match {
      case i: Identifier => Success[Expr](i)
      case _ => Failure[Expr](isIdentifier)
    }
  }

  case class isEqualTo[P](x: Expr) extends Strategy[P] {
    def apply(e: P): RewriteResult[P] = if (e == x) Success(e) else Failure(isEqualTo(x))
  }

  case class contains[P](x: Expr) extends Strategy[Expr] {
    def apply(e: Expr): RewriteResult[Expr] = oncetd(isEqualTo(x))(e)
  }
}
