package elevate.lift.strategies

import elevate.core.strategies.traversal.oncetd
import elevate.core.{Failure, RewriteResult, Strategy, Success}
import lift.core.{Expr, Identifier, Lambda, Program}

import scala.language.implicitConversions

object predicate {

  implicit def rewriteResultToBoolean[T <: Program](r: RewriteResult[T]): Boolean = r match {
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

  case class isEqualTo[T <: Program](x: Expr) extends Strategy[T] {
    def apply(e: T): RewriteResult[T] = if (e == x) Success(e) else Failure(isEqualTo(x))
  }

  case class contains[T <: Program](x: Expr) extends Strategy[Expr] {
    def apply(e: Expr): RewriteResult[Expr] = oncetd(isEqualTo(x))(e)
  }
}
