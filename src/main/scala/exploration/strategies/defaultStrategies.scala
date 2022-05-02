package exploration.strategies

import elevate.core._
import elevate.core.strategies.basic._
import rise.elevate.Rise
import elevate.core.strategies.debug.debug
import elevate.core.strategies.traversal._
import elevate.macros.RuleMacro.rule
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.lowering._
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.algorithmic.reorder
import rise.elevate.strategies.lowering._
import rise.elevate.strategies.normalForm._
import rise.elevate.strategies.predicate._
import rise.elevate.strategies.tiling._
import rise.elevate.strategies.traversal
import rise.elevate.strategies.traversal._
import rise.elevate.NormalizedThen

// scalastyle:off
object defaultStrategies {

  val outermost: (Strategy[Rise]) => (Strategy[Rise]) => Strategy[Rise] =
    traversal.outermost(default.RiseTraversable)
  val innermost: (Strategy[Rise]) => (Strategy[Rise]) => Strategy[Rise] =
    traversal.innermost(default.RiseTraversable)


  // -- BASELINE ---------------------------------------------------------------


  @rule def baseline: Strategy[Rise] = DFNF()(default.RiseTraversable) `;`
    fuseReduceMap `@` topDown[Rise]


  // -- BLOCKING ---------------------------------------------------------------

  val isFullyAppliedReduce: Strategy[Rise] = isApplied(isApplied(isApplied(isReduce)))

  @rule def blocking: Strategy[Rise] =
    baseline `;`
      (tile(32, 32) `@` outermost(mapNest(2))) `;;`
      (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
      (splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;`
      reorder(List(1, 2, 5, 6, 3, 4))


  // -- VECTORIZATION ----------------------------------------------------------

  val isFullyAppliedMap: Strategy[Rise] = isApplied(isApplied(isMap))

  @rule def vectorization: Strategy[Rise] =
    blocking `;;`
      (vectorize(32) `@` innermost(isApplied(isApplied(isMap))))


  // -- LOOP PERMUTATION -------------------------------------------------------

  @rule def loopPerm: Strategy[Rise] =
    baseline `;`
      (tile(32, 32) `@` outermost(mapNest(2))) `;;`
      (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
      (splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;`
      reorder(List(1, 2, 5, 3, 6, 4)) `;;`
      (vectorize(32) `@` innermost(isFullyAppliedMap))


  // -- ARRAY PACKING ----------------------------------------------------------

  val isTransposedB: Strategy[Rise] = isApplied(isTranspose)
  val permuteB: Strategy[Rise] =
    splitJoin2(32) `;` DFNF() `;` argument(idAfter) `;`
      topDown(liftId()) `;` topDown(createTransposePair) `;` RNF() `;`
      argument(argument(idAfter)) `;` normalize.apply(liftId()) `;`
      topDown(idToCopy)

  val packB: Strategy[Rise] =
    storeInMemory(isTransposedB,
      permuteB `;;`
        (vectorize(32) `@` innermost(isFullyAppliedMap)) `;;`
        (parallel() `@` outermost(isApplied(isMap)))
    ) `@` inLambda

  def inLambda(s: Strategy[Rise]): Strategy[Rise] =
    isLambda `;` ((e: Rise) => body(inLambda(s))(e)) <+ s

  @rule def arrayPacking: Strategy[Rise] = packB `;;` loopPerm


  // -- CACHE BLOCKS -----------------------------------------------------------

  //    arrayPacking `;;` debug[Rise]("after arrayPacking") `;`
  @rule def cacheBlocks: Strategy[Rise] = (
    arrayPacking `;;`
      (unroll `@` innermost(isReduceSeq))
    )


  // -- PARALLEL ---------------------------------------------------------------

  @rule def par: Strategy[Rise] = (
    arrayPacking `;;`
      ((parallel() `@` outermost(isApplied(isMap))) `@`
        outermost(isApplied(isLet))) `;;`
      (unroll `@` innermost(isReduceSeq))
    )

  val strategies: Set[Strategy[Rise]] = Set(
    blocking,
    vectorization,
    loopPerm,
    arrayPacking,
    cacheBlocks,
    par
  )

  val lowering = lowerToC

}
