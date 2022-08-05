package explorations

import apps.tvmGemm
import apps.tvmGemm.{innermost, outermost}
//import exploration.{ExecutorConfig, MetaheuristicConfig, runner, uniqueFilename}
import elevate.heuristic_search.ExplorationResult
import exploration._
import explorations.explorationTutorial.mm
import rise.elevate.strategies.normalForm.DFNF
import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.heuristic_search.util._
import elevate.macros.RuleMacro.rule
import exploration.neighborhoods._
import exploration.runner.{CExecutor, checkExpressionC}
import rise.core.DSL.{fun, lf32}
import rise.core.primitives.{add, fst, map, reduce, snd, transpose, zip}
import rise.core.types.DataType.{ArrayType, f32}
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

class mmCPU_exploration extends test_util.Tests {

  // define expression
  val N = 512

  val mm: Rise = //infer(
    fun(ArrayType(N, ArrayType(N, f32)))(a =>
      fun(ArrayType(N, ArrayType(N, f32)))(b =>
        a |> map(fun(ak =>
          transpose(b) |> map(fun(bk =>
            zip(ak)(bk) |>
              map(fun(x => fst(x) * snd(x))) |>
              reduce(add)(lf32(0.0f))
          ))
        ))
      ))

  // define search space
  @rule def baseline: Strategy[Rise] = DFNF()(default.RiseTraversable) `;`
    (fuseReduceMap `@` topDown[Rise])

  // -- BLOCKING ---------------------------------------------------------------

  val isFullyAppliedReduce: Strategy[Rise] = isApplied(isApplied(isApplied(isReduce)))

  @rule def blocking: Strategy[Rise] =
    baseline `;`
      (tile(32, 32) `@` outermost(mapNest(2))) `;;`
      (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
      (splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;`
      reorder(List(1, 2, 5, 6, 3, 4))

  val blockingPartial1: Strategy[Rise] =
    baseline `;`
      (tile(32, 32) `@` outermost(mapNest(2)))
  val blockingPartial2: Strategy[Rise] =
    baseline `;`
      (tile(32, 32) `@` outermost(mapNest(2))) `;;`
      (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq))))
  val blockingPartial3: Strategy[Rise] =
    baseline `;`
      (tile(32, 32) `@` outermost(mapNest(2))) `;;`
      (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
      (splitStrategy(4) `@` innermost(isFullyAppliedReduce))

  // -- VECTORIZATION ----------------------------------------------------------

  val isFullyAppliedMap: Strategy[Rise] = isApplied(isApplied(isMap))

  @rule def vectorization: Strategy[Rise] =
    blocking `;;`
      (vectorize(32) `@` innermost(isApplied(isApplied(isMap))))

  // -- LOOP PERMUTATION -------------------------------------------------------

  @rule def loopPerm: Strategy[Rise] = baseline `;`
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
  //  @rule def arrayPacking: Strategy[Rise] = packB `;` DFNF()

  // -- CACHE BLOCKS -----------------------------------------------------------

  @rule def cacheBlocks: Strategy[Rise] = (
    arrayPacking `;;` // elevate.core.strategies.debug.debug[Rise]("after arrayPacking") `;`
      (unroll `@` innermost(isReduceSeq))
    )

  // -- PARALLEL ---------------------------------------------------------------

  @rule def par: Strategy[Rise] = (
    //    arrayPacking `;;`
    tvmGemm.loopPerm `;;`
      (parallel() `@` outermost(isApplied(isMap))) `;;`
      (unroll `@` innermost(isReduceSeq))
    )

  @rule def tiling: Strategy[Rise] =
    (tile(32, 32) `@` outermost(mapNest(2))) `;;`
      (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
      (splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;`
      reorder(List(1, 2, 5, 6, 3, 4))

  @rule def tilingPerm: Strategy[Rise] =
    (tile(32, 32) `@` outermost(mapNest(2))) `;;`
      (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
      (splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;`
      reorder(List(1, 2, 5, 3, 6, 4))

  @rule def reorderTiling: Strategy[Rise] =
    (splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;`
      reorder(List(1, 2, 5, 6, 3, 4))

  @rule def reorderLoopPerm: Strategy[Rise] =
    (splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;`
      reorder(List(1, 2, 5, 3, 6, 4))


  val parallel_lowering: scala.collection.immutable.Seq[Strategy[Rise]] = {
    scala.collection.immutable.Seq(
      rise.elevate.rules.lowering.mapParCompute()
    )
  }

  // start from baseline
  val coarse: scala.collection.immutable.Seq[Strategy[Rise]] = {
    scala.collection.immutable.Seq(
      fuseReduceMap,
      tiling,
      tilingPerm,
      vectorize(32),
      rise.elevate.rules.lowering.unroll
    )
  }

  val parallel_coarse: scala.collection.immutable.Seq[Strategy[Rise]] = {
    scala.collection.immutable.Seq(
      fuseReduceMap,
      tiling,
      tilingPerm,
      vectorize(32),
      unroll,
      mapParCompute()
    )
  }

  val fine: scala.collection.immutable.Seq[Strategy[Rise]] = {
    scala.collection.immutable.Seq(
      fuseReduceMap,
      tile(32, 32),
      reduceMapFission(),
      reorderTiling,
      reorderLoopPerm,
      vectorize(32),
      unroll,
    )
  }

  val fine_light: scala.collection.immutable.Seq[Strategy[Rise]] = {
    // cannot at id, everywhere would crash it
    scala.collection.immutable.Seq(
      fuseReduceMap, // tiling block
      tile(32, 32),
      reduceMapFission(),
      reorderTiling,
    )
  }

  val parallel_fine: scala.collection.immutable.Seq[Strategy[Rise]] = {
    scala.collection.immutable.Seq(
      fuseReduceMap,
      tile(32, 32),
      reduceMapFission(),
      reorderTiling,
      reorderLoopPerm,
      vectorize(32),
      unroll,
      mapParCompute()
    )
  }

  val parallel_fine_light: scala.collection.immutable.Seq[Strategy[Rise]] = {
    // cannot at id, everywhere would crash it
    scala.collection.immutable.Seq(
      fuseReduceMap, // tiling block
      tile(32, 32),
      reduceMapFission(),
      reorderTiling,
      mapParCompute() // map par block
    )
  }

  //  ignore("rewrite blocking step by step") {
  test("execute blocking") {

    // create gold expression
    //    val mm_par = par.apply(mm).get
    //    val gold = lowerToC.apply(mm_par).get
    val lowering = fuseReduceMap `@` everywhere `;` lowerToC

    val mm_par = blocking.apply(mm).get

    val gold = lowering.apply(mm_par).get


    val executor = CExecutor(
      lowering = lowering,
      output = "/home/jo/development/experiments/exploration/dodekarch/plot/rewrite_steps",
      iterations = 101,
      goldExpression = gold,
      inputSize = N,
      saveToDisk = false,
      timeout = 10000
    )
    //
    //    val blockingPartial1: Strategy[Rise] =
    //      baseline `;`
    //        (tile(32, 32) `@` outermost(mapNest(2)))
    //    val blockingPartial2: Strategy[Rise] =
    //      baseline `;`
    //        (tile(32, 32) `@` outermost(mapNest(2))) `;;`
    //        (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq))))
    //    val blockingPartial3: Strategy[Rise] =
    //      baseline `;`
    //        (tile(32, 32) `@` outermost(mapNest(2))) `;;`
    //        (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
    //        (splitStrategy(4) `@` innermost(isFullyAppliedReduce))
    //
    //
    //    val e0 = mm
    //    val e1 = exploration.strategies.blockingExploration.blocking_step0.apply(e0).get
    //    val e2 = exploration.strategies.blockingExploration.blocking_step1.apply(e1).get
    //    val e3 = exploration.strategies.blockingExploration.blocking_step2.apply(e2).get
    //    val e4 = exploration.strategies.blockingExploration.blocking_step3.apply(e3).get

    val blocked = blocking.apply(mm).get

    var output = scala.collection.immutable.Seq.empty[ExplorationResult[Rise]]

    Range(0, 10).foreach(elem => {

      output = output :+ executor.execute(
        Solution[Rise](
          solutionSteps = scala.collection.immutable.Seq(
            SolutionStep[Rise](
              expression = blocked,
              strategy = blocking,
              location = 0
            )
          )
        )
      )
    })

    val min = output.map(elem => elem.performance.get).sorted.head
    val max = output.map(elem => elem.performance.get).sorted.last
    val variance = executor.variance(output.toSeq.map(elem => elem.performance.get))

    println("\n")

    println("Min: " + min)
    println("Max: " + max)
    println("Range: " + (max - min).toString)
    println("Percent min/max: " + min / max)
    println("Percent max/min: " + max / min)
    println("Variance: " + variance)

  }

  ignore("rewrite step by step") {

    //    val mm_par = par.apply(mm).get
    //    val gold = lowerToC.apply(mm_par).get
    //
    //    val lowering = fuseReduceMap `@` everywhere `;` lowerToC
    //
    //    val executor = CExecutor(
    //      lowering = lowering,
    //      output = "/home/jo/development/experiments/exploration/dodekarch/plot/rewrite_steps",
    //      iterations = 10,
    //      goldExpression = gold,
    //      inputSize = N,
    //      saveToDisk = true,
    //      timeout = 10000
    //    )
    //
    //    val e0 = (DFNF()(default.RiseTraversable) `;` (fuseReduceMap `@` topDown[Rise])).apply(mm).get
    //    println("e0: " + executor.execute(Solution[Rise](e0, scala.collection.immutable.Seq(fuseReduceMap))))
    //
    //    val e1 = ((tile(32, 32) `@` outermost(mapNest(2))) `;` DFNF()).apply(e0).get
    //    println("e1: " + executor.execute(Solution[Rise](e1, scala.collection.immutable.Seq(tile(32, 32)))))
    //
    //    val e2 = ((reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq))))).apply(e1).get
    //    println("e2: " + executor.execute(Solution[Rise](e2, scala.collection.immutable.Seq(reduceMapFission()))))
    //
    //    val e3 = ((splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;` DFNF()).apply(e2).get
    //    println("e3: " + executor.execute(Solution[Rise](e3, scala.collection.immutable.Seq(splitStrategy(4)))))
    //
    //    val e4 = (reorder(List(1, 2, 5, 6, 3, 4)) `;` DFNF()).apply(e3).get
    //    println("e4: " + executor.execute(Solution[Rise](e4, scala.collection.immutable.Seq(reorder(List(1, 2, 5, 6, 3, 4))))))
    //
    //    val e5 = ((vectorize(32) `@` innermost(isFullyAppliedMap)) `;` DFNF()).apply(e4).get
    //    println("e5: " + executor.execute(Solution[Rise](e5, scala.collection.immutable.Seq(vectorize(32)))))
    //
    //    val e6 = ((parallel() `@` outermost(isApplied(isMap))) `;` DFNF()).apply(e5).get
    //    println("e6: " + executor.execute(Solution[Rise](e6, scala.collection.immutable.Seq(parallel()))))
    //
    //    val e7 = (unroll `@` innermost(isReduceSeq)).apply(e6).get
    //    println("e7: " + executor.execute(Solution[Rise](e7, scala.collection.immutable.Seq(unroll))))

  }

  test("mmCPU - parallel_fine_light") {

    val e = mm

    val ii = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "IterativeImprovement",
        depth = 5,
        samples = 1000
      )
    )

    val localSearch = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "localSearch",
        depth = 5,
        samples = 1000
      )
    )

    val annealing = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "annealing",
        depth = 5,
        samples = 3000,
        repeat = 1
      )
    )

    val tabuSearch = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "tabuSearch",
        depth = 5,
        samples = 10000
      )
    )

    val tabuSearchPlain = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "tabuSearchPlain",
        depth = 5,
        samples = 100
      )
    )

    val random = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Random",
        depth = 5,
        samples = 100,
        repeat = 5
      )
    )

    val random_ii = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Random",
        depth = 5,
        samples = 500,
        repeat = 1
      ),
      MetaheuristicConfig(
        heuristic = "IterativeImprovement",
        depth = 5,
        samples = 1000
      )
    )

    val autotuner = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "autotuner",
        depth = 3,
        samples = 10,
        repeat = 1
      )
    )

    val exhaustive = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "exhaustive",
        depth = 5,
        samples = 10000,
      )
    )

    val simulatedAnnealingPlain = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "simulatedAnnealingPlain",
        depth = 5, // ignored?
        samples = 500, // ignored?
      )
    )

    val experiment = scala.collection.immutable.Seq(
      //      annealing,
      //      tabuSearch,
      //      autotuner
      //      tabuSearch
      //      random_ii
      exhaustive,
      //      ii,
      //      random
      //      localSearch,
      //      tabuSearchPlain
      //      simulatedAnnealingPlain
    )

    val executor = ExecutorConfig(
      name = "C",
      iterations = 11,
      threshold = 10
    )

    // define neighborhood style
    val nTreeChildren = NeighborhoodConfig(
      neighborhood = NTreeChildrenChoice
    )

    val nTreeLeafsDistance = NeighborhoodConfig(
      neighborhood = NTreeLeafsDistanceChoice
    )

    val neighborhood = nTreeChildren

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "mmCPU_fine_light",
      output = "/home/jo/development/experiments/exploration/dodekarch/parallel_fine_light",
      inputSize = N,
      metaheuristics = Right(experiment),
      executor = executor,
      lowering = exploration.strategies.blockingExploration.lowering,
      strategies = fine_light,
      //      rewriteFunction = Some(exploration.rewriter.everywhere.rewriteFunction(parallel_fine_light)), // maybe call neighborhood deep
      neighborhoodConfig = neighborhood, // what about window (mabye add a config here as well)
      // todo check whether this can be removed
      rewriteFunction = Some(
        exploration.rewriter.everywhere.neighbourhoodWide(
          strategies = fine_light,
          slideWindow = 20
        )
      ),
      checkExpression = Some(
        checkExpressionC(
          exploration.strategies.blockingExploration.lowering
        )
      ),
      normalForm = Some(DFNF()), // after rewrite
      importExport = None,
      expert = Some(2.676716),
      //                  expert = Some(0.992146)
      //      expert = Some(1.931617), // expert for non-turbo thinkpad for 256
      overwrite = false
    )

    // repeat this
    val explorationResult = exploration.explore(explorer)(e)

    // plot
    // python experiment/plot_experiment2.py /home/jo/development/experiments/exploration/dodekarch/dodekarch/parallel_fine_light/mmCPU_parallel_fine_light

  }
}
