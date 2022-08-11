package explorations

import apps.tvmGemm
import apps.tvmGemm.{innermost, outermost}
import exploration.runner.DebugExecutor
import exploration.strategies.blockingExploration
import util.{assertSame, gen}
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
import rise.elevate.strategies.algorithmic.reorder2
import rise.elevate.strategies.lowering._
import rise.elevate.strategies.normalForm._
import rise.elevate.strategies.predicate._
import rise.elevate.strategies.tiling._
import rise.elevate.strategies.traversal
import rise.elevate.strategies.traversal._

class mmCPU_exploration extends test_util.Tests {

  // define expression
  val N = 1024

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

  // unfused
  val mm_lowered: Rise =
    fun(ArrayType(N, ArrayType(N, f32)))(a =>
      fun(ArrayType(N, ArrayType(N, f32)))(b =>
        a |> rise.core.primitives.mapSeq(fun(ak =>
          transpose(b) |> rise.core.primitives.mapSeq(fun(bk =>
            zip(ak)(bk) |>
              rise.core.primitives.mapSeq(fun(x => fst(x) * snd(x))) |> rise.core.primitives.toMem |> // required copy ?
              rise.core.primitives.reduceSeq(add)(lf32(0.0f))
          ))
        ))
      ))

  // todo
  // define lowering mm->mm_lowered

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

  // warning: introduces fusion of reduce and map
  // therefore fission in the and at everywhere
  // but not if we applied the map fusion rule?
  @rule def reorderTiling: Strategy[Rise] =
    (splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;`
      reorder(List(1, 2, 5, 6, 3, 4)) `;;` reduceMapFission() `@` everywhere

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

  test("test unfused lowering") {

    // define different versions
    val handwritten = mm_lowered
    val rewritten = exploration.strategies.blockingExploration.lowering.apply(mm).get
    val fused = lowerToC.apply(baseline(mm).get).get

    // generate code from different version
    val handwritten_codegen = gen.c.function("riseFun").asStringFromExpr(handwritten)
    val rewritten_codegen = gen.c.function("riseFun").asStringFromExpr(rewritten)
    val fused_codegen = gen.c.function("riseFun").asStringFromExpr(fused)

    // count
    val priceHandwritten = exploration.runner.performanceModel(handwritten)
    val priceRewritten = exploration.runner.performanceModel(rewritten)
    val priceFused = exploration.runner.performanceModel(fused)

    // print
    println("handwritten_codegen: \n" + handwritten_codegen)
    println("rewritten_codegen: \n" + rewritten_codegen)
    println("fused_codegen: \n" + fused_codegen)

    println("Handwritten: " + priceHandwritten)
    println("Rewritten: " + priceRewritten)
    println("Fused: " + priceFused)

    assert(priceRewritten == priceHandwritten)
    assert(priceFused != priceHandwritten)
  }

  test("execute baseline") {
    val lowering = fuseReduceMap `@` everywhere `;` lowerToC
    val gold = lowering.apply(blocking.apply(mm).get).get

    val executor = CExecutor(
      lowering = lowering,
      output = "/home/jo/development/experiments/exploration/thinkjo",
      iterations = 11,
      goldExpression = gold,
      inputSize = N,
      saveToDisk = false,
      timeout = 10000
    )

    var output = scala.collection.immutable.Seq.empty[ExplorationResult[Rise]]

    Range(0, 10).foreach(_ => {

      output = output :+ executor.execute(
        Solution[Rise](
          solutionSteps = scala.collection.immutable.Seq(
            SolutionStep[Rise](
              expression = mm,
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


  test("execute baseline unfused") {

    // ignore lowering
    //    val lowering = elevate.core.strategies.basic.id[Rise]
    val lowering = exploration.strategies.blockingExploration.lowering

    val gold = mm_lowered

    val executor = CExecutor(
      lowering = lowering,
      output = "/home/jo/development/experiments/exploration/thinkjo",
      iterations = 11,
      goldExpression = gold,
      inputSize = N,
      saveToDisk = false,
      timeout = 10000
    )

    var output = scala.collection.immutable.Seq.empty[ExplorationResult[Rise]]

    Range(0, 10).foreach(_ => {

      output = output :+ executor.execute(
        Solution[Rise](
          solutionSteps = scala.collection.immutable.Seq(
            SolutionStep[Rise](
              expression = mm,
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

  test("rewrite step by step") {

    //    val mm_par = par.apply(mm).get
    ////    val gold = lowerToC.apply(mm_par).get
    ////
    //////    val lowering = fuseReduceMap `@` everywhere `;` lowerToC
    //////
    ////////    val executor = CExecutor(
    ////////      lowering = lowering,
    ////////      output = "/home/jo/development/experiments/exploration/dodekarch/plot/rewrite_steps",
    ////////      iterations = 10,
    ////////      goldExpression = gold,
    ////////      inputSize = N,
    ////////      saveToDisk = true,
    ////////      timeout = 10000
    //    )

    // intermediate fission

    println("mm: \n" + mm)

    // fuse reduce map
    val a0 = (DFNF()(default.RiseTraversable) `;` (fuseReduceMap `@` topDown[Rise])).apply(mm).get // fuse
    val a1 = ((tile(32, 32) `@` outermost(mapNest(2))) `;` DFNF()).apply(a0).get // tile
    //    println("a1: \n" + a1)
    val a2 = ((reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq))))).apply(a1).get // fission
    println("a2: \n" + a2)
    //    val a3 = ((splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;` DFNF()).apply(a2).get // split
    //    println("a3: \n" + a3)

    //    val a4 = (reorder2(List(1, 2, 5, 6, 3, 4)) `;` DFNF()).apply(a3).get // reorder and don't fuse
    //    println("a4: \n" + a4)

    //    val a4 = (reorder(List(1, 2, 5, 6, 3, 4)) `;` DFNF()).apply(a3).get // reorder and don't fuse
    //    println("a4: \n" + a4)

    //    val a5 = (reduceMapFission() `@` everywhere).apply(a4).get // normalize?
    //    val a5 = normalize(fuseReduceMap `@).apply(a4).get
    //    println("a5: \n" + a5)
    //
    //    val a6 = (fuseReduceMap `@` everywhere).apply(a5).get
    //    println("a6: \n" + a6)
    //
    //    // no intermediate fission
    val b0 = mm // no fuse
    val b1 = ((tile(32, 32) `@` outermost(mapNest(2))) `;` DFNF()).apply(b0).get // tile
    println("b1: \n" + b1)

    val lowered = blockingExploration.lowering.apply(b1).get

    val code = gen.c.function("riseFun").asStringFromExpr(lowered)
    println("code: \n" + code)

    //    val b2 = b1 // no fission necessary
    //    val b3 = ((splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;` DFNF()).apply(b2).get // split
    //    println("b3: \n" + b3)
    //    val b4 = (reorder(List(1, 2, 5, 6, 3, 4)) `;` DFNF()).apply(b3).get // reorder
    //    println("b4: \n" + b4)

    // new search space
    // fuse reduce map // fuse
    // tile // tile somewhere
    // fission because of blocked reduce
    // split strategy + reordering // requires to be fissions introduces fusion
    // optional parallel

  }

  val fine_light_updated: scala.collection.immutable.Seq[Strategy[Rise]] = {
    // cannot at id, everywhere would crash it
    scala.collection.immutable.Seq(
      fuseReduceMap, // fuse (stand alone optimization)
      tile(32, 32), // add fuse before and fission after // fuse should not be a requirement for (tile)
      reduceMapFission(), // steps required to get reordering to work as we split the reduce into a blocked reduce (makes sense)
      reorderTiling, // split + reordering (one step, split is not very generic)
    )
  }

  // high level rules
  // fuse
  // tiling
  // loop permutation // generic (tiled and untiled)
  // parallel
  // vectorize (on CPU does not make much difference (seems to, maybe check with -O0)
  // unroll (might cause problems as OpenMP seems to handle parallel loops better


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
      output = "/home/jo/development/experiments/exploration/thinkjo/parallel_fine_light",
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
