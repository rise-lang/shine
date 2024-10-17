package explorations

import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.macros.RuleMacro.rule
import exploration.neighborhoods._
import exploration.{ExecutorConfig, MetaheuristicConfig, NeighborhoodConfig}
import rise.core.DSL._
import rise.core.primitives.{let => _}
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.lowering._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.traversal._
import rise.elevate.{Rise, tunable}

class rq_1 extends test_util.Tests {

  // @rule broken here? why?
  @rule def splitJoinRule: Strategy[Rise] = tunable(splitJoin)

  //  @rule def mapGlobalRule: Strategy[Rise] = tunable(rise.elevate.rules.lowering.mapGlobal)

  @rule def vectorizeRule: Strategy[Rise] = tunable(rise.elevate.rules.lowering.vectorize)

  val rules2: scala.collection.immutable.Seq[Strategy[Rise]] = {
    scala.collection.immutable.Seq(
      elevate.core.strategies.basic.id[Rise], // should be applicable?
    )
  }

  // define rules
  val rules_no_vectorization: scala.collection.immutable.Seq[Strategy[Rise]] = {

    scala.collection.immutable.Seq(
      // fusion
      fuseReduceMap,

      // split join, vectorize
      splitJoinRule,
      //      vectorizeRule,

      // map
      rise.elevate.rules.lowering.mapGlobal(0),
      rise.elevate.rules.lowering.mapGlobal(1),
      rise.elevate.rules.lowering.mapWorkGroup(0),
      rise.elevate.rules.lowering.mapWorkGroup(1),
      rise.elevate.rules.lowering.mapLocal(0),
      rise.elevate.rules.lowering.mapLocal(1),
      rise.elevate.rules.lowering.mapSeqCompute(),
      rise.elevate.rules.lowering.mapSeq,
      //      rise.elevate.rules.lowering.mapSeqUnroll,

      // reduce
      //      rise.elevate.rules.lowering.reduceSeq,
      //      rise.elevate.rules.lowering.reduceSeqUnroll,
      rise.elevate.rules.lowering.oclReduceSeqPrivate,
      rise.elevate.rules.lowering.oclReduceSeqLocal,
      rise.elevate.rules.lowering.oclReduceSeqGlobal,
      //      rise.elevate.rules.lowering.oclReduceSeqUnrollPrivate,
      //      rise.elevate.rules.lowering.oclReduceSeqUnrollLocal,
      //      rise.elevate.rules.lowering.oclReduceSeqUnrollGlobal,

      //      rise.elevate.rules.lowering.ocl.circularBuffer(AddressSpace.Private),
      //      rise.elevate.rules.lowering.ocl.circularBuffer(AddressSpace.Local),
      //      rise.elevate.rules.lowering.ocl.circularBuffer(AddressSpace.Global),


      //      rise.elevate.rules.lowering.ocl.rotateValues2,
      rise.elevate.rules.lowering.ocl.oclRotateValuesPrivate,
      rise.elevate.rules.lowering.ocl.oclRotateValuesLocal,
      rise.elevate.rules.lowering.ocl.oclRotateValuesGlobal,
      // unroll
      //      rise.elevate.rules.lowering.unroll,

    )
  }

  // define rules no split join
  val rules_no_split_join: scala.collection.immutable.Seq[Strategy[Rise]] = {

    scala.collection.immutable.Seq(
      // fusion
      fuseReduceMap,

      // split join, vectorize
      //      splitJoinRule,
      //      vectorizeRule,

      // map
      rise.elevate.rules.lowering.mapGlobal(0),
      rise.elevate.rules.lowering.mapGlobal(1),
      rise.elevate.rules.lowering.mapWorkGroup(0),
      rise.elevate.rules.lowering.mapWorkGroup(1),
      rise.elevate.rules.lowering.mapLocal(0),
      rise.elevate.rules.lowering.mapLocal(1),
      rise.elevate.rules.lowering.mapSeqCompute(),
      rise.elevate.rules.lowering.mapSeq,
      //      rise.elevate.rules.lowering.mapSeqUnroll,

      // reduce
      //      rise.elevate.rules.lowering.reduceSeq,
      //      rise.elevate.rules.lowering.reduceSeqUnroll,
      rise.elevate.rules.lowering.oclReduceSeqPrivate,
      rise.elevate.rules.lowering.oclReduceSeqLocal,
      rise.elevate.rules.lowering.oclReduceSeqGlobal,
      //      rise.elevate.rules.lowering.oclReduceSeqUnrollPrivate,
      //      rise.elevate.rules.lowering.oclReduceSeqUnrollLocal,
      //      rise.elevate.rules.lowering.oclReduceSeqUnrollGlobal,

      //      rise.elevate.rules.lowering.ocl.circularBuffer(AddressSpace.Private),
      //      rise.elevate.rules.lowering.ocl.circularBuffer(AddressSpace.Local),
      //      rise.elevate.rules.lowering.ocl.circularBuffer(AddressSpace.Global),

      //      rise.elevate.rules.lowering.ocl.rotateValues2,
      rise.elevate.rules.lowering.ocl.oclRotateValuesPrivate,
      rise.elevate.rules.lowering.ocl.oclRotateValuesLocal,
      rise.elevate.rules.lowering.ocl.oclRotateValuesGlobal,
      // unroll
      //      rise.elevate.rules.lowering.unroll,

    )

  }

  // define rules
  val rules: scala.collection.immutable.Seq[Strategy[Rise]] = {

    scala.collection.immutable.Seq(
      // fusion
      fuseReduceMap,

      // split join, vectorize
      splitJoinRule,
      vectorizeRule,

      // map
      rise.elevate.rules.lowering.mapGlobal(0),
      rise.elevate.rules.lowering.mapGlobal(1),
      rise.elevate.rules.lowering.mapWorkGroup(0),
      rise.elevate.rules.lowering.mapWorkGroup(1),
      rise.elevate.rules.lowering.mapLocal(0),
      rise.elevate.rules.lowering.mapLocal(1),
      rise.elevate.rules.lowering.mapSeqCompute(),
      rise.elevate.rules.lowering.mapSeq,
      //      rise.elevate.rules.lowering.mapSeqUnroll,

      // reduce
      //      rise.elevate.rules.lowering.reduceSeq,
      //      rise.elevate.rules.lowering.reduceSeqUnroll,
      rise.elevate.rules.lowering.oclReduceSeqPrivate,
      rise.elevate.rules.lowering.oclReduceSeqLocal,
      rise.elevate.rules.lowering.oclReduceSeqGlobal,
      //      rise.elevate.rules.lowering.oclReduceSeqUnrollPrivate,
      //      rise.elevate.rules.lowering.oclReduceSeqUnrollLocal,
      //      rise.elevate.rules.lowering.oclReduceSeqUnrollGlobal,

      //      rise.elevate.rules.lowering.ocl.circularBuffer(AddressSpace.Private),
      //      rise.elevate.rules.lowering.ocl.circularBuffer(AddressSpace.Local),
      //      rise.elevate.rules.lowering.ocl.circularBuffer(AddressSpace.Global),

      //      rise.elevate.rules.lowering.ocl.rotateValues2,
      rise.elevate.rules.lowering.ocl.oclRotateValuesPrivate,
      rise.elevate.rules.lowering.ocl.oclRotateValuesLocal,
      rise.elevate.rules.lowering.ocl.oclRotateValuesGlobal,
      // unroll
      //      rise.elevate.rules.lowering.unroll,

    )


  }

  //  default lowering that always wins
  val lowering0: Strategy[Rise] =
    addCopiesForUnfusedReduceOcl `@` everywhere

  val lowering1: Strategy[Rise] = rise.elevate.rules.lowering.reduceSeq `@` everywhere

  val lowering2: Strategy[Rise] =
    rise.elevate.rules.lowering.reduceOCL()

  val lowering3: Strategy[Rise] =
    rise.elevate.rules.lowering.mapGlobal(0) `@` topDown[Rise]

  val lowering4: Strategy[Rise] =
    rise.elevate.rules.lowering.mapGlobal(1) `@` topDown[Rise]

  val lowering5: Strategy[Rise] =
    rise.elevate.rules.lowering.mapSeqCompute() `@` normalize[Rise]

  val lowering6: Strategy[Rise] = rise.elevate.rules.lowering.ocl.oclRotateValuesPrivate `@` normalize[Rise]

  val lowering: Strategy[Rise] = {
    lowering0 `;` // add copies if necessary
      lowering1 `;` // reduce -> reduceSeq
      lowering2 `;` // reduceSeq -> reduceOcl
      //      lowering3 `;` // map -> map global 0 (topdown/outermost)
      //        lowering4 // map -> mapGlobal 1 (topdown/outermost)
      lowering5 `;` // map (compute) -> mapSeq
      lowering6
  }

  test("scal") {

    val mcts = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "MCTS",
        depth = 6,
        samples = 1000,
        repeat = 5
      )
    )

    val localSearch = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "LocalSearch",
        depth = 10,
        samples = 1000,
        repeat = 5
      )
    )

    val randomGraph = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "RandomGraph",
        depth = 6,
        samples = 1000,
        repeat = 5
      )
    )

    val exhaustive = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Exhaustive",
        depth = 10000, // is this ignored? -> maybe not for tree window slide
        samples = 1000, // 7 hours for 1000 samples // 18 hours 5000
        repeat = 1
      )
    )

    val executor = ExecutorConfig(
      name = "AutoTuning",
      iterations = 51, // execution iterations
      threshold = 10, // speedup to cut iterations
      samples = 50, // samples per tuning run
      global_size_limit = 1024,
    )

    val experiment = scala.collection.immutable.Seq(
      exhaustive,
      randomGraph,
      localSearch,
      mcts,
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "scal",
      output = "/home/jo/shine/experiments/exploration/gcloud/rq1",
      inputSizes = scala.collection.immutable.Seq(scal.inputSize),
      metaheuristics = Right(experiment),
      executor = executor,
      lowering = lowering,
      strategies = rules_no_vectorization, // is this ignored here?
      hostCode = Some(scal.hostCode),
      neighborhoodConfig = NeighborhoodConfig(neighborhood = NGraphChoice),
      rewriteFunction = None,
      normalForm = None,
      importExport = None,
      expert = Some(5.0),
      default = Some(1000.000),
      overwrite = false
    )

    val result = exploration.explore(explorer)(scal.expression)
  }


  test("kmeans") {

    val mcts = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "MCTS",
        depth = 6,
        samples = 1000,
        repeat = 5
      )
    )

    val localSearch = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "LocalSearch",
        depth = 10,
        samples = 1000,
        repeat = 5
      )
    )
    val randomGraph = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "RandomGraph",
        depth = 6,
        samples = 1000,
        repeat = 5
      )
    )

    val exhaustive = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Exhaustive",
        depth = 10000, // does not matter
        samples = 1000, // how long?
        repeat = 1
      )
    )

    val executor = ExecutorConfig(
      name = "AutoTuning",
      iterations = 51, // execution iterations
      threshold = 10, // speedup to cut iterations
      samples = 50, // samples per tuning run
      global_size_limit = 1024,
    )

    val experiment = scala.collection.immutable.Seq(
      exhaustive,
      //      randomGraph,
      localSearch,
      mcts
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "kmeans",
      output = "/home/jo/shine/experiments/exploration/gcloud/rq1",
      inputSizes = scala.collection.immutable.Seq(kmeans.p, kmeans.c, kmeans.f),
      metaheuristics = Right(experiment),
      executor = executor,
      lowering = lowering,
      strategies = rules, // is this ignored here?
      hostCode = Some(kmeans.hostCode),
      neighborhoodConfig = NeighborhoodConfig(neighborhood = NGraphChoice),
      rewriteFunction = None,
      normalForm = None,
      importExport = None,
      expert = Some(1),
      default = Some(1000.000),
      overwrite = false
    )

    val result = exploration.explore(explorer)(kmeans.expression)

  }


  test("mm") {

    val mcts = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "MCTS",
        depth = 6,
        samples = 1000,
        repeat = 5
      )
    )

    val localSearch = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "LocalSearch",
        depth = 10,
        samples = 1000,
        repeat = 5
      )
    )
    val randomGraph = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "RandomGraph",
        depth = 6,
        samples = 1000,
        repeat = 5
      )
    )

    val exhaustive = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Exhaustive",
        depth = 1000, // is this ignored? -> maybe not for tree window slide
        samples = 1000, // 7 hours for 1000 samples // 18 hours 5000
        repeat = 1
      )
    )

    val experiment = scala.collection.immutable.Seq(
      exhaustive,
      randomGraph,
      localSearch,
      mcts,
    )

    val executor = ExecutorConfig(
      name = "AutoTuning",
      iterations = 51, // execution iterations
      threshold = 10, // speedup to cut iterations
      samples = 50, // samples per tuning run
      global_size_limit = 1024,
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "mm",
      output = "/home/jo/shine/experiments/exploration/gcloud/rq1",
      inputSizes = scala.collection.immutable.Seq(mm.N),
      metaheuristics = Right(experiment),
      executor = executor,
      lowering = lowering,
      strategies = rules, // is this ignored here?
      hostCode = Some(mm.hostCode),
      neighborhoodConfig = NeighborhoodConfig(neighborhood = NGraphChoice),
      rewriteFunction = None,
      normalForm = None,
      importExport = None,
      expert = Some(2.3),
      default = Some(21500.000),
      overwrite = false
    )

    val result = exploration.explore(explorer)(mm.expression)
  }


  test("asum lowering exploration") {


    val mcts = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "MCTS",
        depth = 6,
        samples = 1000,
        repeat = 5
      )
    )

    val localSearch = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "LocalSearch",
        depth = 10,
        samples = 1000,
        repeat = 5
      )
    )
    val randomGraph = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "RandomGraph",
        depth = 6,
        samples = 1000,
        repeat = 5
      )
    )

    val exhaustive = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Exhaustive",
        depth = 1000, // ignore
        samples = 1000,
        repeat = 1
      )
    )

    val experiment = scala.collection.immutable.Seq(
      exhaustive,
      randomGraph,
      localSearch,
      mcts,
    )

    val executor = ExecutorConfig(
      name = "AutoTuning",
      iterations = 51, // execution iterations
      threshold = 10, // speedup to cut iterations
      samples = 50, // samples per tuning run
      global_size_limit = 1024,
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "asum",
      output = "/home/jo/shine/experiments/exploration/gcloud/rq1",
      inputSizes = scala.collection.immutable.Seq(asum.inputSize),
      metaheuristics = Right(experiment),
      executor = executor,
      lowering = lowering,
      strategies = rules_no_split_join, // is this ignored here?
      hostCode = Some(asum.hostCode),
      neighborhoodConfig = NeighborhoodConfig(neighborhood = NGraphChoice),
      rewriteFunction = None,
      normalForm = None,
      importExport = None,
      expert = Some(2.3),
      default = Some(21500.000),
      overwrite = false
    )

    val result = exploration.explore(explorer)(asum.nvidiaDerived1)

  }

  // define acoustic stencil experiment here
  test("acoustic stencil") {

    val mcts = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "MCTS",
        depth = 6,
        samples = 1000,
        repeat = 5
      )
    )

    val localSearch = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "LocalSearch",
        depth = 10,
        samples = 1000,
        repeat = 5
      )
    )

    val randomGraph = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "RandomGraph",
        depth = 6,
        samples = 1000,
        repeat = 5
      )
    )

    val exhaustive = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Exhaustive",
        depth = 100, // ignored
        samples = 1000,
        repeat = 1
      )
    )

    val experiment = scala.collection.immutable.Seq(
      exhaustive,
      randomGraph,
      localSearch,
      mcts,
    )

    val executor = ExecutorConfig(
      name = "AutoTuning",
      iterations = 51, // execution iterations
      threshold = 100, // speedup to cut iterations
      samples = 50, // samples per tuning run
      global_size_limit = 1024,
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "acoustic",
      output = "/home/jo/shine/experiments/exploration/gcloud/rq1",
      inputSizes = scala.collection.immutable.Seq(acoustic.N, acoustic.M, acoustic.O), // check how this is used
      metaheuristics = Right(experiment),
      executor = executor,
      lowering = lowering,
      strategies = rules, // is this ignored here?
      hostCode = Some(acoustic.hostCode),
      neighborhoodConfig = NeighborhoodConfig(neighborhood = NGraphChoice),
      rewriteFunction = None,
      normalForm = None,
      importExport = None,
      expert = None,
      default = None,
      overwrite = false
    )

    val result = exploration.explore(explorer)(acoustic.expression)
  }

}