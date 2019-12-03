package elevate.core.strategies

import elevate.core.Strategy

// generic one-level traversal strategies
trait Traversable[P] {

  // apply strategy to all direct subexpressions
  def all: Strategy[P] => Strategy[P]

  // apply strategy to one direct subexpression
  def one: Strategy[P] => Strategy[P]

  // apply strategy to at least one and all other applicable direct subexpressions
  def some: Strategy[P] => Strategy[P]

  // apply strategy to one direct subexpression considering the Failure-state
  // when moving from a failing subexpression to the next available
  def oneUsingState: Strategy[P] => Strategy[P]
}
