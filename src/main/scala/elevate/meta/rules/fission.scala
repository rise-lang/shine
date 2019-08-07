package elevate.meta.rules

import elevate.core.{Elevate, Failure, Lift, Meta, RewriteResult, Strategy, Success}
import elevate.core.strategies.basic._
import elevate.lift.strategies.traversal._

object fission {

  case object bodyFission extends Strategy[Elevate] {
    def apply(e: Elevate): RewriteResult[Elevate] = e match {
      case body(seq(f,s)) => Success(seq(body(f),body(s)))
      case _ => Failure(bodyFission)
    }
    override def toString = "bodyFission"
  }

  case object functionFission extends Strategy[Elevate] {
    def apply(e: Elevate): RewriteResult[Elevate] = e match {
      case function(seq(f,s)) => Success(seq(function(f),function(s)))
      case _ => Failure(functionFission)
    }
    override def toString = "functionFission"
  }

  case object argumentFission extends Strategy[Elevate] {
    def apply(e: Elevate): RewriteResult[Elevate] = e match {
      case argument(seq(f,s)) => Success(seq(argument(f),argument(s)))
      case _ => Failure(argumentFission)
    }
    override def toString = "argumentFission"
  }

  case object argumentOfFission extends Strategy[Elevate] {
    def apply(e: Elevate): RewriteResult[Elevate] = e match {
      case argumentOf(x,seq(f,s)) => Success(seq(argumentOf(x,f), argumentOf(x,s)))
      case _ => Failure(argumentFission)
    }
    override def toString = "argumentFission"
  }

  def FNF: Meta = normalizeElevate(bodyFission <+ functionFission <+ argumentFission <+ argumentOfFission)



}
