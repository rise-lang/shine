package elevate.rise.strategies

import elevate.core.{Failure, RewriteResult, Strategy, Success}
import elevate.rise.Lift
import lift.core.{Identifier, Lambda}

import scala.language.implicitConversions

object predicate {

  case object isLambda extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case l: Lambda => Success(l)
      case _ => Failure(isLambda)
    }
    override def toString = "isLambda"
  }

  case object isNotLambda extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case l: Lambda => Failure(isNotLambda)
      case _ => Success(e)
    }
    override def toString = "isLambda"
  }

  case object isIdentifier extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case i: Identifier => Success[Lift](i)
      case _ => Failure[Lift](isIdentifier)
    }
    override def toString = "isIdentifier"
  }
}
