package elevate.core.strategies

import elevate.core._
import elevate.core.strategies.traversal._
import elevate.core.strategies.basic._
import elevate.lift.rules.algorithmic._
import elevate.lift.rules.movement._
import elevate.lift.rules._
import elevate.lift.strategies.traversal._
import elevate.lift.strategies.normalForm._

object tiling {

  def tile: Int => Strategy = i =>
    tryAll(splitJoin(i)) `;` reductionNormalForm `;`
    rewriteNormalForm `;`
    body(
      body(argument(function( // find out what this pattern resembles?
      body(argument(print `;`
        createTransposePair `;`
        argument(mapMapFBeforeTranspose) `;`
          print)))))) `;`
      rewriteNormalForm `;` reductionNormalForm

  def tiling1: Strategy = splitJoin(32)

  // map(map(f)) -> transpose << map(map(f)) << transpose
  def loopInterchange: Strategy = print `;` etaAbstraction `;` print `;`
    body(createTransposePair) `;` print `;` rewriteNormalForm `;` print `;` body(argument(mapMapFBeforeTranspose))


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
