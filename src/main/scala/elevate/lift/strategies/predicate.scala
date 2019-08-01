package elevate.lift.strategies

import elevate.core.strategies.traversal.oncetd
import elevate.core.{Failure, RewriteResult, Strategy, StrategyT, Success}
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

  object isIdentifier extends StrategyT[Expr] {
    def apply(e: Expr): RewriteResult[Expr] = e match {
      case i: Identifier => Success[Expr](i)
      case _ => Failure[Expr](isIdentifier)
    }
  }

  case class isEqualTo[T <: Program](x: Expr) extends StrategyT[Expr] {
    def apply(e: Expr): RewriteResult[Expr] = if (e == x) Success(e) else Failure(isEqualTo(x))
  }

  case class contains[T <: Program](e: Expr) extends StrategyT[Expr] {
    def apply(e: Expr): RewriteResult[Expr] = oncetd(isEqualTo(e))(e)
  }
}
