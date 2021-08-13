package rise.elevate.meta

import elevate.core.strategies.Traversable
import elevate.core.strategies.basic._
import elevate.core.{Strategy, Success}
import elevate.core.macros.rule
import rise.elevate.Rise
import rise.elevate.rules.traversal.{argument, argumentOf, body, function}

object fission {

  def bodyFission: Strategy[Strategy[Rise]] = rule("bodyFission", {
    case body(Seq(f: Strategy[Rise]@unchecked, s: Strategy[Rise]@unchecked)) => Success(seq(body(f))(body(s)))
  })

  def functionFission: Strategy[Strategy[Rise]] = rule("functionFission", {
    case function(Seq(f: Strategy[Rise]@unchecked, s: Strategy[Rise]@unchecked)) => Success(seq(function(f))(function(s)))
  })

  def argumentFission: Strategy[Strategy[Rise]] = rule("argumentFission", {
    case argument(Seq(f: Strategy[Rise]@unchecked, s: Strategy[Rise]@unchecked)) => Success(seq(argument(f))(argument(s)))
  })

  def argumentOfFission: Strategy[Strategy[Rise]] = rule("argumentOfFission", {
    case argumentOf(x,Seq(f: Strategy[Rise]@unchecked, s: Strategy[Rise]@unchecked)) => Success(seq(argumentOf(x,f))(argumentOf(x,s)))
  })

  // Fissioned-Normal-Form: Every single strategy application starts from the root
  def FNF(using ev: Traversable[Strategy[Rise]]): Strategy[Strategy[Rise]] =
    normalize(bodyFission <+ functionFission <+ argumentFission <+ argumentOfFission)
}
