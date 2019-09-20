package elevate.lift.strategies

import elevate.core.{Failure, Lift, RewriteResult, Strategy, Success}
import lift.core.{Expr, Identifier, Lambda, TypedExpr}

import scala.language.implicitConversions

object predicate {

  case object isLambda extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case l: Lambda => Success(l)
      case l@TypedExpr(Lambda(_,_), _) => Success(l)
      case _ => Failure(isLambda)
    }
    override def toString = "isLambda"
  }

  case object isNotLambda extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case l: Lambda => Failure(isNotLambda)
      case TypedExpr(Lambda(_,_), _) => Failure(isNotLambda)
      case _ => Success(e)
    }
    override def toString = "isLambda"
  }

  case object isIdentifier extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case i: Identifier => Success[Lift](i)
      case i@TypedExpr(Identifier(_), _) => Success[Lift](i)
      case _ => Failure[Lift](isIdentifier)
    }
    override def toString = "isIdentifier"
  }
}
