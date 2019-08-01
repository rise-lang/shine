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

  def isLambda: Strategy = {
    case l:Lambda => Success(l)
    case _ => Failure(isLambda)
  }

  def isIdentifier: Strategy ={
    case i:Identifier => Success(i)
    case _ => Failure(isLambda)
  }

  def isEqualTo(e: Expr): Strategy =
    x => if(x == e) Success(x) else Failure(isEqualTo(e))

  def contains(e: Expr): Strategy = oncetd(isEqualTo(e))
}
