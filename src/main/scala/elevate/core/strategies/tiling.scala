package elevate.core.strategies

import elevate.core.Strategy
import elevate.core.strategies.traversal._
import elevate.core.strategies.liftTraversal._
import elevate.core.strategies.normalforms._
import elevate.core.rules.algorithmic._
import elevate.core.rules.movement._

object tiling {

  def norm: Strategy = reductionNormalform `;` rewriteNormalform

  def tile: Int => Strategy = i =>
    tryAll(splitJoin(i)) `;` reductionNormalform `;`
    rewriteNormalform `;`
    body(
      body(argument(function( // find out what this pattern resembles?
      body(argument(print `;`
        createTransposePair `;`
        argument(mapMapFBeforeTranspose) `;`
          print)))))) `;`
      rewriteNormalform `;` reductionNormalform

  def tiling1: Strategy = splitJoin(32)

  /*

  m
  sm.m

  m.n
  sm.m.n
  sm.m.sn.n
  sm.sn.m.n

  m.n.o
  m.sn.n.o
  m.sn.n.so.o
  m.sn.so.n.o

   */
  def tiling2: Strategy => Strategy = s => print `;`
    argument(s) `;` print `;` s

}
