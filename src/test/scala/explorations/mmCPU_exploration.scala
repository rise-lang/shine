package explorations

import apps.tvmGemm
import apps.tvmGemm.{innermost, outermost}
import exploration.{ExecutorConfig, MetaheuristicConfig, runner, uniqueFilename}
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
      vectorize(31),
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
    scala.collection.immutable.Seq(
      fuseReduceMap,
      tile(32, 32),
      reduceMapFission(),
      reorderTiling,
      mapParCompute()
    )
  }

  ignore("rewrite blocking step by step") {

    val mm_par = par.apply(mm).get
    val gold = lowerToC.apply(mm_par).get

    val lowering = fuseReduceMap `@` everywhere `;` lowerToC

    val executor = CExecutor(
      lowering = lowering,
      output = "/home/jo/development/experiments/exploration/dodekarch/plot/rewrite_steps",
      iterations = 1,
      goldExpression = gold,
      inputSize = N,
      saveToDisk = true,
      timeout = 10000
    )

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


    val e0 = mm
    val e1 = exploration.strategies.blockingExploration.blocking_step0.apply(e0).get
    val e2 = exploration.strategies.blockingExploration.blocking_step1.apply(e1).get
    val e3 = exploration.strategies.blockingExploration.blocking_step2.apply(e2).get
    val e4 = exploration.strategies.blockingExploration.blocking_step3.apply(e3).get

    //    println("e0: " + executor.execute(Solution[Rise](e0, scala.collection.immutable.Seq.empty[Strategy[Rise]])))
    //    println("e1: " + executor.execute(Solution[Rise](e1, scala.collection.immutable.Seq(exploration.strategies.blockingExploration.blocking_step0))))
    //    println("e2: " + executor.execute(Solution[Rise](e2, scala.collection.immutable.Seq(exploration.strategies.blockingExploration.blocking_step1))))
    //    println("e3: " + executor.execute(Solution[Rise](e3, scala.collection.immutable.Seq(exploration.strategies.blockingExploration.blocking_step2))))
    //    println("e4: " + executor.execute(Solution[Rise](e4, scala.collection.immutable.Seq(exploration.strategies.blockingExploration.blocking_step3))))

  }


  ignore("rewrite step by step") {
    //
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

  test("mmCPU - execute versions") {
    // execute version
    val mm_baseline = baseline.apply(mm).get

    val mm_blocking = blocking.apply(mm).get

    val mm_vectorize = vectorization.apply(mm).get

    val mm_loopPerm = loopPerm.apply(mm).get

    val mm_arrayPacking = arrayPacking.apply(mm).get

    val mm_cacheBlocks = cacheBlocks.apply(mm).get

    val mm_par = par.apply(mm).get

    // gold
    val gold = lowerToC.apply(mm_par).get

    val executor = CExecutor(
      lowering = lowerToC,
      output = "exploration",
      iterations = 1000,
      goldExpression = gold,
      inputSize = N,

      saveToDisk = true
    )

    //    val baselineResult = executor.execute(Solution[Rise](mm_baseline, scala.collection.immutable.Seq(baseline)))
    //    println("baseline: " + baselineResult)
    //    assert(baselineResult.performance.isDefined)
    //
    //    val blockingResult = executor.execute(Solution[Rise](mm_blocking, scala.collection.immutable.Seq(blocking)))
    //    println("blocking: " + blockingResult)
    //    assert(blockingResult.performance.isDefined)
    //
    //    val vectorizationResult = executor.execute(Solution[Rise](mm_vectorize, scala.collection.immutable.Seq(vectorization)))
    //    println("vectorization: " + vectorizationResult)
    //    assert(vectorizationResult.performance.isDefined)
    //
    //    val loopPermResult = executor.execute(Solution[Rise](mm_loopPerm, scala.collection.immutable.Seq(loopPerm)))
    //    println("loopPerm: " + loopPermResult)
    //    assert(loopPermResult.performance.isDefined)
    //
    //    val arrayPackingResult = executor.execute(Solution[Rise](mm_arrayPacking, scala.collection.immutable.Seq(arrayPacking)))
    //    println("arrayPacking: " + arrayPackingResult)
    //    assert(arrayPackingResult.performance.isDefined)
    //
    //    val cacheBlocksResult = executor.execute(Solution[Rise](mm_cacheBlocks, scala.collection.immutable.Seq(cacheBlocks)))
    //    println("cacheBlocks: " + cacheBlocksResult)
    //    assert(cacheBlocksResult.performance.isDefined)

    //    val parResult = executor.execute(Solution[Rise](mm_par, scala.collection.immutable.Seq(par)))
    //    println("par: " + parResult)
    //    assert(parResult.performance.isDefined)

  }


  ignore("mmCPU - parallel_array_packing") {

    val e = cacheBlocks.apply(mm).get

    val exhaustive = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "exhaustive",
        depth = 7,
        samples = 500,
      )
    )

    val random = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Random",
        depth = 7,
        samples = 100,
        repeat = 10
      )
    )

    val experiment = scala.collection.immutable.Seq(
      exhaustive,
      random
    )

    val executor = ExecutorConfig(
      name = "C",
      iterations = 10,
      threshold = 2
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "mmCPU_parallel_array_packing",
      output = "/home/jo/development/experiments/exploration/dodekarch/parallel_array_packing",
      inputSize = N,
      metaheuristics = Right(experiment),
      executor = executor,
      lowering = exploration.strategies.blockingExploration.lowering,
      strategies = parallel_lowering,
      rewriteFunction = Some(exploration.rewriter.everywhere.rewriteFunction(parallel_lowering)),
      normalForm = Some(DFNF()),
      importExport = None,
      //      importExport = Some(exploration.explorationUtil.IO.importExport),
      expert = Some(29.955511)
    )

    val explorationResult = exploration.explore(explorer)(e)

    println("explorationResult: " + explorationResult)
  }

  ignore("mmCPU - parallel_tiling") {

    val e = blocking.apply(mm).get

    val exhaustive = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "exhaustive",
        depth = 7,
        samples = 1000,
      )
    )

    val random = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Random",
        depth = 7,
        samples = 100,
        repeat = 10
      )
    )

    val experiment = scala.collection.immutable.Seq(
      exhaustive,
      random
    )

    val executor = ExecutorConfig(
      name = "C",
      iterations = 10,
      threshold = 2
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "mmCPU_parallel_tiling",
      output = "/home/jo/development/experiments/exploration/dodekarch/parallel_tiling",
      inputSize = N,
      metaheuristics = Right(experiment),
      executor = executor,
      lowering = exploration.strategies.blockingExploration.lowering,
      strategies = parallel_lowering,
      rewriteFunction = Some(exploration.rewriter.everywhere.rewriteFunction(parallel_lowering)),
      normalForm = Some(DFNF()),
      importExport = None,
      //      importExport = Some(exploration.explorationUtil.IO.importExport),
      expert = Some(2.676716)
    )

    val explorationResult = exploration.explore(explorer)(e)

    println("explorationResult: " + explorationResult)
  }

  ignore("mmCPU - coarse") {

    // start with pre-baseline version
    val e = mm

    val exhaustive = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "exhaustive",
        depth = 5,
        samples = 1000,
      )
    )

    val random = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Random",
        depth = 5,
        samples = 100, // in total
        repeat = 10
      )
    )

    val experiment = scala.collection.immutable.Seq(
      exhaustive,
      random
    )

    val executor = ExecutorConfig(
      name = "C",
      iterations = 10,
      threshold = 2
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "mmCPU_coarse",
      output = "/home/jo/development/experiments/exploration/dodekarch/coarse",
      inputSize = N,
      metaheuristics = Right(experiment),
      executor = executor,
      lowering = exploration.strategies.blockingExploration.lowering,
      strategies = coarse,
      rewriteFunction = Some(exploration.rewriter.everywhere.rewriteFunction(coarse)),
      normalForm = Some(DFNF()),
      //      importExport = Some(exploration.explorationUtil.IO.importExport),
      importExport = None,
      expert = Some(2.676716)
    )

    val explorationResult = exploration.explore(explorer)(e)

    println("explorationResult: " + explorationResult)

  }

  ignore("mmCPU - parallel_coarse") {

    // start with pre-baseline version
    val e = mm

    val exhaustive = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "exhaustive",
        depth = 6,
        samples = 1000,
      )
    )

    val random = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Random",
        depth = 6,
        samples = 100, // in total
        repeat = 10
      )
    )

    val experiment = scala.collection.immutable.Seq(
      exhaustive,
      random
    )

    val executor = ExecutorConfig(
      name = "C",
      iterations = 10,
      threshold = 2
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "mmCPU_parallel_coarse",
      output = "/home/jo/development/experiments/exploration/dodekarch/parallel_coarse",
      inputSize = N,
      metaheuristics = Right(experiment),
      executor = executor,
      lowering = exploration.strategies.blockingExploration.lowering,
      strategies = parallel_coarse,
      rewriteFunction = Some(exploration.rewriter.everywhere.rewriteFunction(parallel_coarse)),
      normalForm = Some(DFNF()),
      //      importExport = Some(exploration.explorationUtil.IO.importExport),
      importExport = None,
      expert = Some(2.676716)
    )

    val explorationResult = exploration.explore(explorer)(e)

    println("explorationResult: " + explorationResult)

  }

  ignore("mmCPU - fine") {


    // start with pre-baseline version
    val e = mm

    val exhaustive = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "exhaustive",
        depth = 7,
        samples = 1000,
      )
    )

    val random = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Random",
        depth = 7,
        samples = 100, // in total
        repeat = 10
      )
    )

    val experiment = scala.collection.immutable.Seq(
      exhaustive,
      random
    )

    val executor = ExecutorConfig(
      name = "C",
      iterations = 10,
      threshold = 2
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "mmCPU_fine",
      output = "/home/jo/development/experiments/exploration/dodekarch/fine",
      inputSize = N,
      metaheuristics = Right(experiment),
      executor = executor,
      lowering = exploration.strategies.blockingExploration.lowering,
      strategies = fine,
      rewriteFunction = Some(exploration.rewriter.everywhere.rewriteFunction(fine)),
      normalForm = Some(DFNF()),
      importExport = None,
      //      importExport = Some(exploration.explorationUtil.IO.importExport),
      expert = Some(15.947497)
    )

    val explorationResult = exploration.explore(explorer)(e)

    println("explorationResult: " + explorationResult)

  }

  ignore("mmCPU - parallel_fine") {

    // start with pre-baseline version
    //    val e = blockingPartial2.apply(mm).get
    val e = mm

    val ii = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "IterativeImprovement",
        depth = 8,
        samples = 1000
      )
    )


    val random = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Random",
        depth = 8,
        samples = 100,
        repeat = 10
      )
    )

    val exhaustive = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "exhaustive",
        depth = 8,
        samples = 1000,
      )
    )

    val experiment = scala.collection.immutable.Seq(
      exhaustive,
      random
    )

    val executor = ExecutorConfig(
      name = "C",
      iterations = 10,
      threshold = 2
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "mmCPU_parallel_fine",
      output = "/home/jo/development/experiments/exploration/dodekarch/parallel_fine",
      inputSize = N,
      metaheuristics = Right(experiment),
      executor = executor,
      lowering = exploration.strategies.blockingExploration.lowering,
      strategies = parallel_fine,
      rewriteFunction = Some(exploration.rewriter.everywhere.rewriteFunction(parallel_fine)),
      normalForm = Some(DFNF()),
      importExport = None,
      //      importExport = Some(exploration.explorationUtil.IO.importExport),
      expert = Some(2.676716),
    )

    // repeat this
    val explorationResult = exploration.explore(explorer)(e)

    println("explorationResult: " + explorationResult)

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

    val random = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Random",
        depth = 5,
        samples = 500,
        repeat = 1
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
        depth = 5,
        samples = 1000,
        repeat = 10
      )
    )

    val exhaustive = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "exhaustive",
        depth = 5,
        samples = 10000,
      )
    )

    val experiment = scala.collection.immutable.Seq(
      //      annealing,
      tabuSearch,
      //      autotuner
      //      tabuSearch
      //      random_ii
      //      exhaustive,
      //      ii,
      //      random
    )

    val executor = ExecutorConfig(
      name = "C",
      iterations = 11,
      threshold = 10
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "mmCPU_parallel_fine_light",
      output = "/home/jo/development/experiments/exploration/dodekarch/parallel_fine_light",
      inputSize = N,
      metaheuristics = Right(experiment),
      executor = executor,
      lowering = exploration.strategies.blockingExploration.lowering,
      strategies = parallel_fine_light,
      rewriteFunction = Some(exploration.rewriter.everywhere.rewriteFunction(parallel_fine_light)),
      normalForm = Some(DFNF()),
      importExport = None,
      //      importExport = Some(exploration.explorationUtil.IO.importExport),
      //      expert = Some(2.676716),
      //                  expert = Some(0.992146)
      expert = Some(1.931617), // expert for non-turbo thinkpad for 256
      overwrite = true
    )

    // repeat this
    val explorationResult = exploration.explore(explorer)(e)


    // plot
    // python experiment/plot_experiment2.py /home/jo/development/experiments/exploration/dodekarch/dodekarch/parallel_fine_light/mmCPU_parallel_fine_light


  }

  ignore("regex") {

    println(uniqueFilename("hello"))
    println(uniqueFilename("hello_"))
    println(uniqueFilename("hello_1"))
    println(uniqueFilename("hello_1234"))
    println(uniqueFilename("hello_1234_uwe"))
    println(uniqueFilename("test.pdf"))
    println(uniqueFilename("test_0"))

  }
}
