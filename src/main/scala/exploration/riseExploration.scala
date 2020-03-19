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
import exploration.search.CExecutor
import elevate.core.strategies.traversal.{alltd, bottomup, oncebu, oncetd, topdown}
import elevate.core.{Failure, Strategy, Success}
import elevate.heuristic_search.{Metaheuristic}
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
      LCNF `;`  rules.movement.mapTransposeBeforeJoin `;` LCNF,
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

    strategies.foreach(elem => {
      println("strategy: " + elem.toString())
    })



    // parse this from config

    println("initialization started")

    // c Runner with 3 iterations and lowering strategy
    val lowering = elevate.rise.rules.lowering.lowerToC
    val cExecutor = new CExecutor(lowering, 3)

    // create heuristics
    val random = new Random[Rise]
    val iterativeImprovement = new IterativeImprovement[Rise]

    // depth and iterations matters
    val first = new Metaheuristic[Rise]("Random", random, 5, 5, cExecutor, strategies)
    // depth doesn't matter; iterations should be 1
    val main = new Metaheuristic[Rise]("II", iterativeImprovement, 0, 1, first, strategies)

    println("initialization finished")

    // start things
    println("execution started")
    val result = main.execute(solution)
    println("execution finished")

    println("result: " + result._1)
    println("result: " + result._2)

  }
}
