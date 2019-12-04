package elevate.rise.strategies

import elevate.core.{Failure, RewriteResult, Strategy, Success}
import elevate.rise.Rise
import lift.core.{Identifier, Lambda}

import scala.language.implicitConversions

object predicate {

  case object isLambda extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case l: Lambda => Success(l)
      case _ => Failure(isLambda)
    }
    override def toString = "isLambda"
  }

  case object isNotLambda extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case l: Lambda => Failure(isNotLambda)
      case _ => Success(e)
    }
    override def toString = "isLambda"
  }

  case object isIdentifier extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case i: Identifier => Success[Rise](i)
      case _ => Failure[Rise](isIdentifier)
    }
    override def toString = "isIdentifier"
  }
}
