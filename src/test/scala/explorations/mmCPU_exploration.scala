package explorations

import apps.tvmGemm
import apps.tvmGemm.{innermost, outermost}
import exploration.{ExecutorConfig, MetaheuristicConfig}
import explorations.explorationTutorial.mm
import rise.elevate.strategies.normalForm.DFNF
import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.heuristic_search.util.Solution
import elevate.macros.RuleMacro.rule
import exploration.runner.CExecutor
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
    loopPerm `;;`
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

  // define strategies here
  val map_strategies: scala.collection.immutable.Seq[Strategy[Rise]] = {
    scala.collection.immutable.Seq(
      //      tile(32, 32), // part of tiling
      //      reduceMapFission(), // part of tiling
      //      reorderTiling,
      //      reorderLoopPerm,
      //      arrayPacking,
      //      fuseReduceMap,
      //      packB `;;` id,
      //      tiling, // split this into smaller steps
      //      tilingPerm, // split this into smaller steps/pieces
      //      vectorize(32),
      //      rise.elevate.rules.lowering.unroll, // unroll reduce Seq (we can just apply this once)
      //      rise.elevate.rules.lowering.mapSeq, // we don't need this here (default lowering case -> interesting if we want to fuse in lowering) or unroll
      rise.elevate.rules.lowering.mapParCompute() // in default case we have 7 locations to apply this
    )
  }

  val fine_tiling_and_map_strategies: scala.collection.immutable.Seq[Strategy[Rise]] = {
    scala.collection.immutable.Seq(
      fuseReduceMap, // has no effect (is covered in lowering)
      tile(32, 32), // huge amount of options
      reduceMapFission(),
      (splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;` reorder(List(1, 2, 5, 6, 3, 4)), // todo split this into separate strategies
      (splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;` reorder(List(1, 2, 5, 3, 6, 4)), //  warning, we will have an invalid result
      vectorize(32),
      unroll,
      mapParCompute() // lowering, rest will be mapSeq (or map) implicitly
    )
  }

  //
  //  @rule def reorder_blocking = ((splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;` reorder(List(1, 2, 5, 6, 3, 4))) // todo split this into separate strategies
  //
  //
  //  @rule def reorder_loopPerm = ((splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;` reorder(List(1, 2, 5, 3, 6, 4))) //  warning, we will have an invalid result


  // very unlikely that sequence is found by random sampling
  val fine_tiling_strategies: scala.collection.immutable.Seq[Strategy[Rise]] = {
    scala.collection.immutable.Seq(
      fuseReduceMap, // has no effect (is covered in lowering)
      tile(32, 32), // huge amount of options
      reduceMapFission(), // one option
      (splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;` reorder(List(1, 2, 5, 6, 3, 4)), // todo split this into separate strategies
      (splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;` reorder(List(1, 2, 5, 3, 6, 4)), //  warning, we will have an invalid result
      vectorize(32),
      unroll,
    )
  }

  val tiling_and_map_strategies: scala.collection.immutable.Seq[Strategy[Rise]] = {
    scala.collection.immutable.Seq(
      fuseReduceMap,
      tiling, // split this into smaller steps
      tilingPerm, // split this into smaller steps/pieces
      vectorize(32),
      rise.elevate.rules.lowering.unroll, // unroll reduce Seq (we can just apply this once)
      //      rise.elevate.rules.lowering.mapSeq, // we don't need this here (default lowering case -> interesting if we want to fuse in lowering) or unroll
      rise.elevate.rules.lowering.mapParCompute() // in default case we have 7 locations to apply this
    )
  }

  ignore("mmCPU - execute versions") {
    // execute version
    val mm_baseline = baseline.apply(mm).get

    val mm_blocking = blocking.apply(mm).get
    val mm_blocking_lowered = lowerToC.apply(mm_blocking).get

    val mm_vectorize = vectorization.apply(mm).get

    val mm_loopPerm = loopPerm.apply(mm).get
    val mm_loopPerm_lowered = lowerToC.apply(mm_loopPerm).get

    val mm_arrayPacking = arrayPacking.apply(mm).get
    val mm_arrayPacking_lowered = lowerToC.apply(mm_arrayPacking).get

    val mm_cacheBlocks = cacheBlocks.apply(mm).get

    val mm_par = par.apply(mm).get
    val mm_par_lowered = lowerToC.apply(mm_par).get

    val executor = CExecutor(
      lowering = lowerToC,
      output = "/home/jo/development/experiments/exploration/dodekarch/plot/baselines",
      iterations = 100,
      goldExpression = mm_par_lowered,
      inputSize = N,
      saveToDisk = true
    )

    val baselineResult = executor.execute(Solution[Rise](mm_baseline, scala.collection.immutable.Seq(baseline)))
    println("baseline: " + baselineResult)
    assert(baselineResult.performance.isDefined)

    val blockingResult = executor.execute(Solution[Rise](mm_blocking, scala.collection.immutable.Seq(blocking)))
    println("blocking: " + blockingResult)
    assert(blockingResult.performance.isDefined)

    val vectorizationResult = executor.execute(Solution[Rise](mm_vectorize, scala.collection.immutable.Seq(vectorization)))
    println("vectorization: " + vectorizationResult)
    assert(vectorizationResult.performance.isDefined)

    val loopPermResult = executor.execute(Solution[Rise](mm_loopPerm, scala.collection.immutable.Seq(loopPerm)))
    println("loopPerm: " + loopPermResult)
    assert(loopPermResult.performance.isDefined)

    val arrayPackingResult = executor.execute(Solution[Rise](mm_arrayPacking, scala.collection.immutable.Seq(arrayPacking)))
    println("arrayPacking: " + arrayPackingResult)
    assert(arrayPackingResult.performance.isDefined)

    val cacheBlocksResult = executor.execute(Solution[Rise](mm_cacheBlocks, scala.collection.immutable.Seq(cacheBlocks)))
    println("cacheBlocks: " + cacheBlocksResult)
    assert(cacheBlocksResult.performance.isDefined)

    val parResult = executor.execute(Solution[Rise](mm_par, scala.collection.immutable.Seq(par)))
    println("par: " + parResult)
    assert(parResult.performance.isDefined)

  }


  test("mmCPU - explore maps") {

    val e = arrayPacking.apply(mm).get

    val ii = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "IterativeImprovement",
        depth = 4,
        samples = 1
      )
    )

    val random = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Random",
        depth = 10,
        samples = 1
      )
    )

    val autotuner = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "autotuner",
        depth = 13,
        samples = 1
      )
    )

    val exhaustive = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "exhaustive",
        depth = 7,
        samples = 10000,
      )
    )

    val executor = ExecutorConfig(
      name = "C",
      iterations = 10,
      threshold = 2
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "mmCPU_maps",
      output = "/home/jo/development/experiments/exploration/dodekarch/maps",
      inputSize = N,
      metaheuristics = exhaustive,
      executor = executor,
      lowering = exploration.strategies.blockingExploration.lowering,
      strategies = exploration.strategies.blockingExploration.strategies,
      rewriteFunction = Some(exploration.rewriter.everywhere.rewriteFunction(map_strategies)),
      normalForm = Some(DFNF()),
      importExport = Some(exploration.explorationUtil.IO.importExport)
    )

    val explorationResult = exploration.explore(explorer)(e)

    println("explorationResult: " + explorationResult)
  }


  test("mmCPU - explore tiling and maps") {

    // start with pre-baseline version
    val e = mm

    val ii = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "IterativeImprovement",
        depth = 4,
        samples = 1
      )
    )

    val random = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Random",
        depth = 10,
        samples = 1000 // in total
      ),
      MetaheuristicConfig(
        heuristic = "IterativeImprovement",
        depth = 5, // is ignored I guess
        samples = 1
      )
    )

    val autotuner = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "autotuner",
        depth = 13,
        samples = 1
      )
    )

    val exhaustive = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "exhaustive",
        depth = 12,
        samples = 10000,
      )
    )

    val executor = ExecutorConfig(
      name = "C",
      iterations = 10,
      threshold = 2
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "mmCPU_tiling_and_maps",
      output = "/home/jo/development/experiments/exploration/dodekarch/tiling_and_maps",
      inputSize = N,
      metaheuristics = exhaustive,
      executor = executor,
      lowering = exploration.strategies.blockingExploration.lowering,
      strategies = exploration.strategies.blockingExploration.strategies,
      rewriteFunction = Some(exploration.rewriter.everywhere.rewriteFunction(tiling_and_map_strategies)),
      normalForm = Some(DFNF()),
      importExport = Some(exploration.explorationUtil.IO.importExport)
    )

    val explorationResult = exploration.explore(explorer)(e)

    println("explorationResult: " + explorationResult)

  }

  test("mmCPU - explore fine tiling") {


    // start with pre-baseline version
    val e = mm

    val ii = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "IterativeImprovement",
        depth = 10,
        samples = 1
      )
    )

    val random = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Random",
        depth = 5,
        samples = 250
      )
      ,
      MetaheuristicConfig(
        heuristic = "IterativeImprovement",
        depth = 3,
        samples = 1
      )
    )

    val exhaustive = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "exhaustive",
        depth = 7,
        samples = 10000,
      )
    )

    val autotuner = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "autotuner",
        depth = 4,
        samples = 1
      )
    )

    val executor = ExecutorConfig(
      name = "C",
      iterations = 5,
      threshold = 2
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "mmCPU_fine_tiling",
      output = "/home/jo/development/experiments/exploration/dodekarch/fine_tiling",
      inputSize = N,
      metaheuristics = exhaustive,
      executor = executor,
      lowering = exploration.strategies.blockingExploration.lowering,
      strategies = exploration.strategies.blockingExploration.strategies,
      rewriteFunction = Some(exploration.rewriter.everywhere.rewriteFunction(fine_tiling_strategies)),
      normalForm = Some(DFNF()),
      importExport = Some(exploration.explorationUtil.IO.importExport)
    )

    val explorationResult = exploration.explore(explorer)(e)

    println("explorationResult: " + explorationResult)

  }


  ignore("mmCPU - explore fine tiling and maps") {

    // start with pre-baseline version
    val e = mm

    val ii = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "IterativeImprovement",
        depth = 4,
        samples = 1
      )
    )

    val random = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Random",
        depth = 5,
        samples = 1000
      )
      //      ,
      //      MetaheuristicConfig(
      //        heuristic = "IterativeImprovement",
      //        depth = 5,
      //        iteration = 1
      //      )
    )

    val exhaustive = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "exhaustive",
        depth = 500,
        samples = 1,
      )
    )

    val autotuner = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "autotuner",
        depth = 4,
        samples = 1
      )
    )

    val executor = ExecutorConfig(
      name = "C",
      iterations = 10,
      threshold = 2
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "mmCPU_fine_tiling_and_maps",
      output = "/home/jo/development/experiments/exploration/dodekarch/fine_tiling_and_maps",
      inputSize = N,
      metaheuristics = random,
      executor = executor,
      lowering = exploration.strategies.blockingExploration.lowering,
      strategies = exploration.strategies.blockingExploration.strategies,
      rewriteFunction = Some(exploration.rewriter.everywhere.rewriteFunction(fine_tiling_and_map_strategies)),
      normalForm = Some(DFNF()),
      importExport = Some(exploration.explorationUtil.IO.importExport)
    )

    val explorationResult = exploration.explore(explorer)(e)

    println("explorationResult: " + explorationResult)

  }


}
