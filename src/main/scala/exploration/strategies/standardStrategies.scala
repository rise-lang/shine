package exploration.strategies

import elevate.core.strategies.traversal.{oncebu, oncetd}
import elevate.core.{Strategy}
import elevate.rise.rules.algorithmic.{blockedReduce, fissionReduceMap, fuseReduceMap}
import elevate.rise.rules.movement.{liftReduce, mapFBeforeSlide}
import elevate.rise.{Rise, rules}
import elevate.rise.rules.traversal.LiftTraversable
import elevate.rise.strategies.normalForm.{LCNF, RNF}
import elevate.rise.strategies.tiling
import elevate.rise.strategies.tiling.tileNDList

object standardStrategies {

    // blocking strategy: to be moved!

    // prepare tiling, keep reduce(+) and map(*) close together, (necessary?)
    val fusedReduceMap: Strategy[Rise] = LCNF `;` oncetd(fuseReduceMap)

    // M.N.m.n.K (or in TVM terms: xo,yo,xi,yi,k)
    val tiledOuterTwo: Strategy[Rise] = fusedReduceMap `;`
      oncetd(tileNDList(List(32,32))) `;` LCNF

    // M.N.m.n.K.k (tile K-loop),
    // fission first to enable blocking the reduction loop
    val splitK: Strategy[Rise] = tiledOuterTwo `;`
      oncetd(fissionReduceMap) `;` oncetd(blockedReduce(4)) `;` LCNF

    // move the split (blocking the reduction loop)
    // to prepare fusing map(*) and reduce(+) again
    val prepareFusion: Strategy[Rise] = splitK `;`
      oncetd(mapFBeforeSlide) `;` LCNF

    // move map(*) into both reduce loops again
    val fusedReduceMapAgain: Strategy[Rise] = prepareFusion `;`
      oncetd(fuseReduceMap) `;` LCNF `;` oncetd(fuseReduceMap) `;` LCNF

    // move outer K loop up M.N.m.K.n.k
    val moveOuterKLoopOnce: Strategy[Rise] = fusedReduceMapAgain `;`
      RNF `;` oncetd(liftReduce) `;` LCNF

    // move outer K loop further up M.N.K.m.n.k
    val moveOuterKLoopTwice: Strategy[Rise] = moveOuterKLoopOnce `;`
      RNF `;` oncetd(liftReduce) `;` LCNF

    // move inner K loop further up M.N.K.m.k.n,
    val moveInnerKLoopOnce: Strategy[Rise] = moveOuterKLoopTwice `;`
      RNF `;` oncebu(liftReduce) `;` LCNF

    val blocking : Strategy[Rise] = moveInnerKLoopOnce `;`
      RNF `;` oncebu(liftReduce)

    val strategies = Set(
      LCNF `;` blocking `;` LCNF,
      LCNF `;` rules.algorithmic.splitJoin(8) `;` LCNF,
      LCNF `;` rules.algorithmic.mapLastFission `;` LCNF,
      LCNF `;` rules.algorithmic.mapFusion `;` LCNF,
      LCNF `;` rules.algorithmic.liftId `;` LCNF,
      LCNF `;` rules.algorithmic.idAfter `;` LCNF,
      LCNF `;` rules.algorithmic.createTransposePair `;` LCNF,
      LCNF `;` rules.algorithmic.removeTransposePair `;` LCNF,
      LCNF `;` rules.algorithmic.slideSeqFusion `;` LCNF,
      LCNF `;` rules.movement.joinBeforeJoin `;` LCNF,
      LCNF `;` rules.movement.joinBeforeMapF `;` LCNF,
      LCNF `;` rules.movement.joinBeforeTranspose `;` LCNF,
      LCNF `;` rules.movement.mapFBeforeSlide `;` LCNF,
      LCNF `;` rules.movement.mapJoinBeforeJoin `;` LCNF,
      LCNF `;` rules.movement.mapJoinBeforeTranspose `;` LCNF,
      LCNF `;` rules.movement.mapTransposeBeforeJoin `;` LCNF,
      LCNF `;` rules.movement.transposeBeforeSlide `;` LCNF,
      LCNF `;` rules.movement.transposeBeforeMapMapF `;` LCNF,
      LCNF `;` rules.movement.transposeBeforeMapJoin `;` LCNF,
      LCNF `;` tiling.loopInterchange `;` LCNF,
      LCNF `;` tiling.tileND(32)(32) `;` LCNF,
      LCNF `;` tiling.tilingExternal `;` LCNF,
      LCNF `;` fusedReduceMap `;` LCNF,
      LCNF `;` tiledOuterTwo `;` LCNF,
      LCNF `;` splitK `;` LCNF,
      LCNF `;` prepareFusion `;` LCNF,
      LCNF `;` fusedReduceMapAgain `;` LCNF,
      LCNF `;` moveOuterKLoopOnce `;` LCNF,
      LCNF `;` moveInnerKLoopOnce `;` LCNF `;` LCNF
    )

  }
