package elevate.rise.meta

import elevate.core.strategies.basic._
import elevate.core.{Failure, MetaStrategy, RewriteResult, Strategy, Success}
import elevate.rise.Rise
import traversal._
import elevate.rise.rules.traversal.{argument, argumentOf, body, function}

object fission {

  case object bodyFission extends MetaStrategy[Rise] {
    def apply(e: Strategy[Rise]): RewriteResult[Strategy[Rise]] = e match {
      case body(seq(f,s)) => Success(seq(body(f),body(s)))
      case _ =>              Failure(bodyFission)
    }
    override def toString = "bodyFission"
  }

  case object functionFission extends MetaStrategy[Rise] {
    def apply(e: Strategy[Rise]): RewriteResult[Strategy[Rise]] = e match {
      case function(seq(f,s)) => Success(seq(function(f),function(s)))
      case _                  => Failure(functionFission)
    }
    override def toString = "functionFission"
  }

  case object argumentFission extends Strategy[Strategy[Rise]] {
    def apply(e: Strategy[Rise]): RewriteResult[Strategy[Rise]] = e match {
      case argument(seq(f,s)) => Success(seq(argument(f),argument(s)))
      case _                  => Failure(argumentFission)
    }
    override def toString = "argumentFission"
  }

  case object argumentOfFission extends Strategy[Strategy[Rise]] {
    def apply(e: Strategy[Rise]): RewriteResult[Strategy[Rise]] = e match {
      case argumentOf(x,seq(f,s)) => Success(seq(argumentOf(x,f), argumentOf(x,s)))
      case _                      => Failure(argumentFission)
    }
    override def toString = "argumentFission"
  }




  // Fissioned-Normal-Form: Every single strategy application starts from the root
  def FNF: MetaStrategy[Rise] = normalize.apply(bodyFission <+ functionFission <+ argumentFission <+ argumentOfFission)
}
