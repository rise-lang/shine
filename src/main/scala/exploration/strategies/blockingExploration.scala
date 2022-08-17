package exploration.strategies

import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.macros.RuleMacro.rule
import rise.elevate.{NormalizedThen, Rise, tunable}
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

// scalastyle:off
object blockingExploration {

  // helper
  val outermost: (Strategy[Rise]) => (Strategy[Rise]) => Strategy[Rise] =
    traversal.outermost(default.RiseTraversable)
  val innermost: (Strategy[Rise]) => (Strategy[Rise]) => Strategy[Rise] =
    traversal.innermost(default.RiseTraversable)


  // lowering
  // maps inside map reduce will stay maps instead of mapSeqs
  //  val lowering =
  //  addRequiredCopies() `;`
  //    fuseReduceMap2 `;` // fuse map and reduce
  //    rise.elevate.rules.lowering.specializeSeq() `;` // lower: map -> mapSeq, reduce -> reduceSeq
  //    reduceMapFission2 `;` // fission map and reduce
  //    rise.elevate.rules.lowering.specializeSeqReduce() // lower: reduce -> reduceSeq
  //    reduceOCL() // lower: reduceSeq -> oclReduceSeq(AddressSpace.Private)

  //  val lowering = fuseReduceMap `@` everywhere `;` lowerToC
  val lowering = addCopiesForUnfusedReduce `@` everywhere `;` lowerToC
  //  val lowering = lowerToC


  // -- BASELINE ---------------------------------------------------------------


  @rule def baseline: Strategy[Rise] = DFNF()(default.RiseTraversable) `;`
    fuseReduceMap `@` topDown[Rise]


  // -- BASELINE -- generic

  // build something like @everywhere?
  // apply with all traversals?

  // -- BLOCKING ---------------------------------------------------------------

  val isFullyAppliedMap: Strategy[Rise] = isApplied(isApplied(isMap))
  val isFullyAppliedReduce: Strategy[Rise] = isApplied(isApplied(isApplied(isReduce)))

  //  @rule def blocking: Strategy[Rise] =
  //    baseline `;`
  //      (tile(32, 32) `@` outermost(mapNest(2))) `;;`
  //      (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
  //      (splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;`
  //      reorder(List(1, 2, 5, 6, 3, 4))

  // todo make traversal more generic


  // blocking (including baseline)
  @rule def blocking_step0: Strategy[Rise] = baseline `;` DFNF() // fusion

  @rule def blocking_step1: Strategy[Rise] = DFNF() `;` (tile(32, 32) `@` outermost(mapNest(2))) `;` DFNF() // tile

  @rule def blocking_step2: Strategy[Rise] = (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;` DFNF() // reduceMapFission

  @rule def blocking_step3: Strategy[Rise] = (splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;` reorder(List(1, 2, 5, 6, 3, 4)) `;` DFNF() // splitStrategy

  // vectorization
  @rule def vectorization_step0: Strategy[Rise] = (vectorize(32) `@` innermost(isApplied(isApplied(isMap))))

  // loop permutation
  @rule def loopPerm_step0: Strategy[Rise] = (tile(32, 32) `@` outermost(mapNest(2))) `;` DFNF() // blocking0

  @rule def loopPerm_step1: Strategy[Rise] = (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;` DFNF() // blocking1

  @rule def loopPerm_step2: Strategy[Rise] = (splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;` reorder(List(1, 2, 5, 3, 6, 4)) `;` DFNF()

  @rule def loopPerm_step4: Strategy[Rise] = (vectorize(32) `@` innermost(isFullyAppliedMap)) // vectorize0

  // array packing

  // -- ARRAY PACKING ----------------------------------------------------------

  val isTransposedB: Strategy[Rise] = isApplied(isTranspose)

  val permuteB: Strategy[Rise] =
    splitJoin2(32) `;` DFNF() `;` argument(idAfter) `;`
      topDown(liftId()) `;` topDown(createTransposePair) `;` RNF() `;`
      argument(argument(idAfter)) `;` normalize.apply(liftId()) `;`
      topDown(idToCopy)

  //  @rule def permuteB0: Strategy[Rise] = splitJoin2(32) `;` DFNF()
  //
  //  @rule def permuteB1: Strategy[Rise] = argument(idAfter)
  //
  //  @rule def permuteB2: Strategy[Rise] = topDown(liftId())
  //
  //  @rule def permuteB3: Strategy[Rise] = topDown(createTransposePair) `;` RNF()
  //
  //  @rule def permuteB4: Strategy[Rise] = argument(argument(idAfter)) `;` normalize.apply(liftId())
  //
  //  @rule def permuteB5: Strategy[Rise] = topDown(idToCopy)

  // todo make this generic steps?
  @rule def packB: Strategy[Rise] =
    storeInMemory(isTransposedB,
      permuteB `;;`
        (vectorize(32) `@` innermost(isFullyAppliedMap)) `;;`
        (parallel() `@` outermost(isApplied(isMap)))
    ) `@` inLambda

  def inLambda(s: Strategy[Rise]): Strategy[Rise] =
    isLambda `;` ((e: Rise) => body(inLambda(s))(e)) <+ s

  //  @rule def arrayPacking: Strategy[Rise] = packB `;;` loopPerm


  val strategies: scala.collection.immutable.Seq[Strategy[Rise]] = scala.collection.immutable.Seq(
    //    baseline,
    blocking_step0,
    blocking_step1, // can rewrite but not execute
    blocking_step2,
    blocking_step3,
    //    vectorization_step0,
    //    loopPerm_step2,
    //    packB
    //    loopPerm_step0,
    //    loopPerm_step1,
    //    loopPerm_step3,
    //    loopPerm_step4
  )


  @rule def splitStrategos: Strategy[Rise] = (splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;` reorder(List(1, 2, 5, 6, 3, 4)) // splitStrategy

  @rule def splitStrategos2: Strategy[Rise] = (tunable(splitStrategy) `@` innermost(isFullyAppliedReduce)) `;;` reorder(List(1, 2, 5, 6, 3, 4)) // splitStrategy


  @rule def tiling: Strategy[Rise] = tile(32, 32)

  @rule def tiling2: Strategy[Rise] = tile()

  val rules: scala.collection.immutable.Seq[Strategy[Rise]] = scala.collection.immutable.Seq(
    fuseReduceMap,
    tiling,
    reduceMapFission(),
    //    splitStrategy(4),
    splitStrategos
    //    (splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;` reorder(List(1, 2, 5, 6, 3, 4)) // splitStrategy
    //    reorder(List(1, 2, 5, 6, 3, 4)),
    //    vectorize(32)
  )

  val rules2: scala.collection.immutable.Seq[Strategy[Rise]] = scala.collection.immutable.Seq(
    fuseReduceMap,
    tile(32, 32),
    reduceMapFission(),
    splitStrategy(4),
    reorder(List(1, 2, 5, 6, 3, 4)),
  )


  val rules3: scala.collection.immutable.Seq[Strategy[Rise]] = scala.collection.immutable.Seq(
    fuseReduceMap,
    tile(),
    reduceMapFission(),
    //    splitStrategy(4),
    splitStrategos2
    //    (splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;` reorder(List(1, 2, 5, 6, 3, 4)) // splitStrategy
    //    reorder(List(1, 2, 5, 6, 3, 4)),
    //    vectorize(32)
  )


  //  val lowering = lowerToC

}
