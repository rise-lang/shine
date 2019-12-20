package elevate.core.strategies

import elevate.core.Strategy

/* Inspired by:

@inproceedings{DBLP:conf/icfp/VisserBT98,
  author    = {Eelco Visser and
               Zine{-}El{-}Abidine Benaissa and
               Andrew P. Tolmach},
  title     = {Building Program Optimizers with Rewriting Strategies},
  booktitle = {{ICFP}},
  pages     = {13--26},
  publisher = {{ACM}},
  year      = {1998}
}

 */

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
