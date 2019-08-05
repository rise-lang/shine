package elevate.lift.strategies

import elevate.core.{Failure, Lift, RewriteResult, Strategy, Success}
import lift.core.{Expr, Identifier, Lambda}

import scala.language.implicitConversions

object predicate {

  def isLambda: Strategy[Lift] = {
    case l:Lambda => Success(l)
    case _ => Failure(isLambda)
  }

  case object isIdentifier extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case i: Identifier => Success[Lift](i)
      case _ => Failure[Lift](isIdentifier)
    }
  }
}
