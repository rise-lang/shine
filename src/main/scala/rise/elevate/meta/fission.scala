package rise.elevate.meta

import elevate.core.strategies.Traversable
import elevate.core.strategies.basic._
import elevate.core.{Strategy, Success}
import elevate.macros.RuleMacro.rule
import rise.elevate.Rise
import rise.elevate.rules.traversal.{argument, argumentOf, body, function}

object fission {

  @rule def bodyFission: Strategy[Strategy[Rise]] = {
    case body(Seq(f, s)) => Success(seq(body(f))(body(s)))
  }

  @rule def functionFission: Strategy[Strategy[Rise]] = {
    case function(Seq(f,s)) => Success(seq(function(f))(function(s)))
  }

  @rule def argumentFission: Strategy[Strategy[Rise]] = {
    case argument(Seq(f,s)) => Success(seq(argument(f))(argument(s)))
  }

  @rule def argumentOfFission: Strategy[Strategy[Rise]] = {
    case argumentOf(x,Seq(f,s)) => Success(seq(argumentOf(x,f))(argumentOf(x,s)))
  }

  // Fissioned-Normal-Form: Every single strategy application starts from the root
  def FNF(implicit ev: Traversable[Strategy[Rise]]): Strategy[Strategy[Rise]] =
    normalize(ev)(bodyFission <+ functionFission <+ argumentFission <+ argumentOfFission)
}
