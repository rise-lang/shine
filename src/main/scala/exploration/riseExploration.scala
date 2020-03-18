package exploration

import elevate.core.Strategy
import elevate.core.strategies.traversal.{oncebu, oncetd}
import elevate.heuristic_search.heuristics.Random
import elevate.rise.rules.algorithmic.{blockedReduce, fissionReduceMap, fuseReduceMap}
import elevate.rise.{Rise, rules}
import elevate.rise.rules.movement.{liftReduce, mapFBeforeSlide}
import elevate.rise.strategies.normalForm.LCNF
import elevate.rise.strategies.tiling
import elevate.rise.strategies.tiling.tileNDList
import exploration.search.{MockupSearch, executeC}
import elevate.core.strategies.traversal.{alltd, bottomup, oncebu, oncetd, topdown}
import elevate.core.{Failure, Strategy, Success}
import elevate.heuristic_search.ProblemConstraints
import elevate.heuristic_search.heuristic.IterativeImprovement
import elevate.rise.rules.algorithmic.{blockedReduce, fissionReduceMap, fuseReduceMap}
import elevate.rise.rules.movement.{liftReduce, mapFBeforeSlide}
import elevate.rise.{Rise, rules}
import elevate.rise.rules.traversal.LiftTraversable
import elevate.rise.strategies.normalForm.{LCNF, RNF}
import elevate.rise.strategies.tiling
import elevate.rise.strategies.tiling.tileNDList


object riseExploration {

  def apply(solution: Rise, lowering: Strategy[Rise]):Unit = {
    val s = solution
    print("initial solution: " + s + "\n")

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
//      blocking,
      rules.algorithmic.splitJoin(8),
      rules.algorithmic.mapLastFission,
      rules.algorithmic.mapFusion,
      rules.algorithmic.liftId,
      rules.algorithmic.idAfter,
      rules.algorithmic.createTransposePair,
      rules.algorithmic.removeTransposePair,
      rules.algorithmic.slideSeqFusion,
      rules.movement.joinBeforeJoin,
      rules.movement.joinBeforeMapF,
      rules.movement.joinBeforeTranspose,
      rules.movement.mapFBeforeSlide,
      rules.movement.mapJoinBeforeJoin,
      rules.movement.mapJoinBeforeTranspose,
      rules.movement.mapTransposeBeforeJoin,
      rules.movement.transposeBeforeSlide,
      rules.movement.transposeBeforeMapMapF,
      rules.movement.transposeBeforeMapJoin,
      tiling.loopInterchange,
//      tiling.tileND(32)(32),
      tiling.tilingExternal,
      fusedReduceMap,
      tiledOuterTwo,
      splitK,
      prepareFusion,
      fusedReduceMapAgain,
      moveOuterKLoopOnce,
      moveInnerKLoopOnce
    )

    strategies.foreach(elem => {
      println("strategy: " + elem.toString())
    })


    // parse this from config

    // C Runner with 3 iterations
    val root = new Runner("C", 0, 3, null , strategies)
    // Random runner with depth 5 and 5 iterations
    val first = new Runner("Random", 5, 5, root, strategies)

    // search version
    val version = new MockupSearch(first, strategies)

    // heuristic
    val iterativeImprovement = new IterativeImprovement[Rise](s, version)
    val resultIterativeImprovement = iterativeImprovement.start()
    println("result Iterative Improvement: " + resultIterativeImprovement)
  }
}
