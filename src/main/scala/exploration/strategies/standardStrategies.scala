package exploration.strategies

import elevate.core._
import elevate.core.strategies.basic._
import rise.elevate.Rise
//import elevate.core.strategies.debug.peek
//import rise.core.IsClosedForm
import elevate.core.strategies.debug.debug
import elevate.core.strategies.traversal._
//import rise.core.TypedDSL._
//import rise.core.types._
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

//import _root_.util.gen

// scalastyle:off
object standardStrategies {

  val outermost: (Strategy[Rise]) => (Strategy[Rise]) => Strategy[Rise] =
    traversal.outermost(default.RiseTraversable)
  val innermost: (Strategy[Rise]) => (Strategy[Rise]) => Strategy[Rise] =
    traversal.innermost(default.RiseTraversable)


  // -- BASELINE ---------------------------------------------------------------

  val baseline: Strategy[Rise] = DFNF()(default.RiseTraversable) `;`
    fuseReduceMap `@` topDown[Rise]


  // -- BLOCKING ---------------------------------------------------------------

  val isFullyAppliedReduce: Strategy[Rise] = isApplied(isApplied(isApplied(isReduce)))
  val blocking: Strategy[Rise] =
//    baseline `;`
      (tile(32,32)        `@` outermost(mapNest(2))) `;;`
      (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
      (splitStrategy(4)   `@` innermost(isFullyAppliedReduce)) `;;`
      reorder(List(1,2,5,6,3,4))


  // -- VECTORIZATION ----------------------------------------------------------

  val isFullyAppliedMap: Strategy[Rise] = isApplied(isApplied(isMap))
  val vectorization: Strategy[Rise] =
    blocking `;;`
      (vectorize(32) `@` innermost(isApplied(isApplied(isMap))))


  // -- LOOP PERMUTATION -------------------------------------------------------

  val loopPerm: Strategy[Rise] =
//    baseline `;`
    (tile(32,32)        `@` outermost(mapNest(2))) `;;`
    (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
    (splitStrategy(4)   `@` innermost(isFullyAppliedReduce)) `;;`
    reorder(List(1,2,5,3,6,4)) `;;`
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
        (parallel()    `@` outermost(isApplied(isMap)))
    ) `@` inLambda

  def inLambda(s: Strategy[Rise]): Strategy[Rise] =
    isLambda `;` ( (e: Rise) => body(inLambda(s))(e) ) <+ s

  val arrayPacking: Strategy[Rise] = packB `;;` loopPerm


  // -- CACHE BLOCKS -----------------------------------------------------------

  val cacheBlocks: Strategy[Rise] = (
    arrayPacking `;;` debug[Rise]("after arrayPacking") `;`
      (unroll `@` innermost(isReduceSeq))
    )


  // -- PARALLEL ---------------------------------------------------------------

  val par = (
    arrayPacking `;;`
      ((parallel() `@` outermost(isApplied(isMap))) `@`
        outermost(isApplied(isLet))) `;;`
      (unroll `@` innermost(isReduceSeq))
    )


  val strategies = Set(
    blocking,
    vectorization,
    loopPerm,
    arrayPacking,
    cacheBlocks,
    par
  )



  //
//    // prepare tiling, keep reduce(+) and map(*) close together, (necessary?)
//    val fusedReduceMap: Strategy[Rise] = LCNF `;` oncetd(fuseReduceMap)
//
//    // M.N.m.n.K (or in TVM terms: xo,yo,xi,yi,k)
//    val tiledOuterTwo: Strategy[Rise] = fusedReduceMap `;`
//      oncetd(tileNDList(List(32,32))) `;` LCNF
//
//    // M.N.m.n.K.k (tile K-loop),
//    // fission first to enable blocking the reduction loop
//    val splitK: Strategy[Rise] = tiledOuterTwo `;`
//      oncetd(fissionReduceMap) `;` oncetd(blockedReduce(4)) `;` LCNF
//
//    // move the split (blocking the reduction loop)
//    // to prepare fusing map(*) and reduce(+) again
//    val prepareFusion: Strategy[Rise] = splitK `;`
//      oncetd(mapFBeforeSlide) `;` LCNF
//
//    // move map(*) into both reduce loops again
//    val fusedReduceMapAgain: Strategy[Rise] = prepareFusion `;`
//      oncetd(fuseReduceMap) `;` LCNF `;` oncetd(fuseReduceMap) `;` LCNF
//
//    // move outer K loop up M.N.m.K.n.k
//    val moveOuterKLoopOnce: Strategy[Rise] = fusedReduceMapAgain `;`
//      RNF `;` oncetd(liftReduce) `;` LCNF
//
//    // move outer K loop further up M.N.K.m.n.k
//    val moveOuterKLoopTwice: Strategy[Rise] = moveOuterKLoopOnce `;`
//      RNF `;` oncetd(liftReduce) `;` LCNF
//
//    // move inner K loop further up M.N.K.m.k.n,
//    val moveInnerKLoopOnce: Strategy[Rise] = moveOuterKLoopTwice `;`
//      RNF `;` oncebu(liftReduce) `;` LCNF
//
//    val blocking : Strategy[Rise] = moveInnerKLoopOnce `;`
//      RNF `;` oncebu(liftReduce)
//
//    val strategies = Set(
//      LCNF `;` blocking `;` LCNF,
//      LCNF `;` rules.algorithmic.splitJoin(8) `;` LCNF,
//      LCNF `;` rules.algorithmic.mapLastFission `;` LCNF,
//      LCNF `;` rules.algorithmic.mapFusion `;` LCNF,
//      LCNF `;` rules.algorithmic.liftId `;` LCNF,
//      LCNF `;` rules.algorithmic.idAfter `;` LCNF,
//      LCNF `;` rules.algorithmic.createTransposePair `;` LCNF,
//      LCNF `;` rules.algorithmic.removeTransposePair `;` LCNF,
//      LCNF `;` rules.algorithmic.slideSeqFusion `;` LCNF,
//      LCNF `;` rules.movement.joinBeforeJoin `;` LCNF,
//      LCNF `;` rules.movement.joinBeforeMapF `;` LCNF,
//      LCNF `;` rules.movement.joinBeforeTranspose `;` LCNF,
//      LCNF `;` rules.movement.mapFBeforeSlide `;` LCNF,
//      LCNF `;` rules.movement.mapJoinBeforeJoin `;` LCNF,
//      LCNF `;` rules.movement.mapJoinBeforeTranspose `;` LCNF,
//      LCNF `;` rules.movement.mapTransposeBeforeJoin `;` LCNF,
//      LCNF `;` rules.movement.transposeBeforeSlide `;` LCNF,
//      LCNF `;` rules.movement.transposeBeforeMapMapF `;` LCNF,
//      LCNF `;` rules.movement.transposeBeforeMapJoin `;` LCNF,
//      LCNF `;` tiling.loopInterchange `;` LCNF,
//      LCNF `;` tiling.tileND(32)(32) `;` LCNF,
//      LCNF `;` tiling.tilingExternal `;` LCNF,
//      LCNF `;` fusedReduceMap `;` LCNF,
//      LCNF `;` tiledOuterTwo `;` LCNF,
//      LCNF `;` splitK `;` LCNF,
//      LCNF `;` prepareFusion `;` LCNF,
//      LCNF `;` fusedReduceMapAgain `;` LCNF,
//      LCNF `;` moveOuterKLoopOnce `;` LCNF,
//      LCNF `;` moveInnerKLoopOnce `;` LCNF `;` LCNF
//    )

  }
