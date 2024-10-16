package explorations

import apps.tvmGemm.{innermost, outermost}
import arithexpr.arithmetic.RangeMul
import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.heuristic_search.util.Solution
import elevate.macros.RuleMacro.rule
import exploration.neighborhoods._
import exploration.runner.{AutoTuningExecutor, CExecutor}
import exploration.{ExecutorConfig, MetaheuristicConfig, NeighborhoodConfig}
import rise.autotune.{HostCode, Tuner, tuningParam, wrapOclRun}
import rise.core.DSL.{fun, lf32}
import rise.core.primitives._
import rise.core.types.DataType.{ArrayType, f32}
import rise.elevate.rules.algorithmic.{joinSplit, _}
import rise.elevate.rules.lowering._
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.algorithmic.reorder
import rise.elevate.strategies.lowering._
import rise.elevate.strategies.normalForm._
import rise.elevate.strategies.predicate._
import rise.elevate.strategies.tiling._
import rise.elevate.strategies.traversal._
import rise.elevate.{NormalizedThen, Rise, tunable}
import rise.core._
import rise.core.DSL._
import rise.core.primitives.{let => _, _}
import rise.core.DSL.Type._
import rise.core.types._
import rise.core.types.DataType._
import rise.openCL.DSL._
import rise.openCL.primitives.oclReduceSeq
import shine.OpenCL.{GlobalSize, LocalSize}
import scala.collection.mutable.ListBuffer

import elevate.heuristic_search.util._
import elevate.heuristic_search.ExplorationResult

class lowering_exploration extends test_util.Tests {


  // @rule broken here? why?
  @rule def splitJoinRule: Strategy[Rise] = tunable(splitJoin)

  //  @rule def mapGlobalRule: Strategy[Rise] = tunable(rise.elevate.rules.lowering.mapGlobal)

  @rule def vectorizeRule: Strategy[Rise] = tunable(rise.elevate.rules.lowering.vectorize)

  val rules2: scala.collection.immutable.Seq[Strategy[Rise]] = {
    scala.collection.immutable.Seq(
      elevate.core.strategies.basic.id[Rise], // should be applicable?
    )
  }


  // make this bidirectional
  val rules_bidirectional: scala.collection.immutable.Seq[Strategy[Rise]] = {

    scala.collection.immutable.Seq(

      fuseReduceMap,
      reduceMapFission(),

      splitJoinRule,
      joinSplit,

      rise.elevate.rules.lowering.mapGlobal(0),
      rise.elevate.rules.lowering.mapGlobal(1),
      rise.elevate.rules.lowering.mapGlobalReverse,

      rise.elevate.rules.lowering.mapWorkGroup(0),
      rise.elevate.rules.lowering.mapWorkGroup(1),
      rise.elevate.rules.lowering.mapWorkGroupReverse,

      rise.elevate.rules.lowering.mapLocal(0),
      rise.elevate.rules.lowering.mapLocal(1),
      rise.elevate.rules.lowering.mapLocalReverse,

      rise.elevate.rules.lowering.mapSeqCompute(),
      rise.elevate.rules.lowering.mapSeq,
      rise.elevate.rules.lowering.mapSeqReverse,

      //      rise.elevate.rules.lowering.oclReduceSeqPrivate,
      //      rise.elevate.rules.lowering.oclReduceSeqLocal,
      //      rise.elevate.rules.lowering.oclReduceSeqGlobal,
      //      rise.elevate.rules.lowering.oclReduceSeqReverse,

      rise.elevate.rules.lowering.oclReducePrivate,
      rise.elevate.rules.lowering.oclReduceLocal,
      rise.elevate.rules.lowering.oclReduceGlobal,
      rise.elevate.rules.lowering.oclReduceReverse,

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

    val exhaustive = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Exhaustive",
        depth = 10000, // is this ignored? -> maybe not for tree window slide
        samples = 1000, // 7 hours for 1000 samples // 18 hours 5000
        repeat = 1
      )
    )

    val localSearch = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "LocalSearch",
        depth = 10,
        samples = 100,
        repeat = 5
      )
    )

    //      randomgraphlocalsearch,
    val mcts = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "MCTS",
        depth = 5,
        samples = 100,
        repeat = 5
      )
    )

    val randomGraph = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "RandomGraph",
        depth = 5,
        samples = 500,
        repeat = 5
      )
    )

    val executor = ExecutorConfig(
      name = "AutoTuning",
      iterations = 11, // execution iterations
      threshold = 10, // speedup to cut iterations
      samples = 10, // samples per tuning run
      global_size_limit = 1024,
    )

    val experiment = scala.collection.immutable.Seq(
      exhaustive,
      localSearch,
      randomGraph,
      //      mcts
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "scal",
      output = "/home/jo/development/rise-lang/shine/experiments/exploration/dodekarch/lowering_exploration",
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

    //    kmeans.compute_gold()

    val result = exploration.explore(explorer)(scal.expression)

  }


  ignore("kmeans") {

    val exhaustive = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Exhaustive",
        depth = 10000, // is this ignored? -> maybe not for tree window slide
        samples = 10000, // 7 hours for 1000 samples // 18 hours 5000
        repeat = 1
      )
    )

    val localSearch = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "LocalSearch",
        depth = 10,
        samples = 100,
        repeat = 5
      )
    )

    //      randomgraphlocalsearch,
    val mcts = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "MCTS",
        depth = 5,
        samples = 100,
        repeat = 5
      )
    )

    val randomGraph = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "RandomGraph",
        depth = 5,
        samples = 1000,
        repeat = 5
      )
    )

    val executor = ExecutorConfig(
      name = "AutoTuning",
      iterations = 11, // execution iterations
      threshold = 10, // speedup to cut iterations
      samples = 10, // samples per tuning run
      global_size_limit = 1024,
    )

    val experiment = scala.collection.immutable.Seq(
      exhaustive,
      //      localSearch,
      //      randomGraph,
      //      mcts
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "kmeans",
      output = "/home/jo/development/rise-lang/shine/experiments/exploration/dodekarch/lowering_exploration",
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

    //    kmeans.compute_gold()

    val result = exploration.explore(explorer)(kmeans.expression)

  }


  //  define mm experiment here
  ignore("mm") {

    val localSearch = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "LocalSearch",
        depth = 10,
        samples = 1000,
        repeat = 5
      )
    )


    //      randomgraphlocalsearch,
    val mcts = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "MCTS",
        depth = 3,
        samples = 1000,
        repeat = 5
      )
    )

    val randomGraph = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "RandomGraph",
        depth = 5,
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
      //      mcts,
      //      randomGraph,
      localSearch,
      exhaustive,
    )

    val executor = ExecutorConfig(
      name = "AutoTuning",
      iterations = 11, // execution iterations
      threshold = 10, // speedup to cut iterations
      samples = 25, // samples per tuning run
      global_size_limit = 1024,
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "mm",
      output = "/home/jo/development/rise-lang/shine/experiments/exploration/dodekarch/lowering_exploration",
      inputSizes = scala.collection.immutable.Seq(mm.N),
      metaheuristics = Right(experiment),
      executor = executor,
      lowering = lowering,
      strategies = rules_bidirectional, // is this ignored here?
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

    //    mm.generate_and_store_gold(mm.N)

  }


  ignore("develop search strategies") {

    val localSearch = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "LocalSearch",
        depth = 10,
        samples = 200,
        repeat = 5
      )
    )

    val annealing = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Annealing",
        depth = 10,
        samples = 200,
        repeat = 1
      )
    )

    val mcts = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "MCTS",
        depth = 10,
        samples = 200,
        repeat = 5
      )
    )
    //
    //    val randomGraphLocalSearch = scala.collection.immutable.Seq(
    //      MetaheuristicConfig(
    //        heuristic = "RandomGraph",
    //        depth = 10,
    //        samples = 200,
    //        repeat = 5
    //      ),
    //      MetaheuristicConfig(
    //        heuristic = "LocalSearch",
    //        depth = 10,
    //        samples = 10,
    //        repeat = 1
    //      ))

    val experiment = scala.collection.immutable.Seq(
      mcts,
      //      localSearch,
      //      randomGraphLocalSearch,
    )

    val executor = ExecutorConfig(
      name = "AutoTuning",
      iterations = 11, // execution iterations
      threshold = 10, // speedup to cut iterations
      samples = 10, // samples per tuning run
      global_size_limit = 1024,
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "asum",
      output = "/home/jo/development/rise-lang/shine/experiments/exploration/dodekarch/lowering_exploration",
      inputSizes = scala.collection.immutable.Seq(asum.inputSize),
      metaheuristics = Right(experiment),
      executor = executor,
      lowering = lowering,
      strategies = rules, // is this ignored here?
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

  ignore("asum lowering exploration") {

    val exhaustive = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Exhaustive",
        depth = 1000, // is this ignored? -> maybe not for tree window slide
        samples = 1000, // 7 hours for 1000 samples // 18 hours 5000
        repeat = 1
      )
    )

    val randomGraph = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "RandomGraph",
        depth = 10,
        samples = 1000,
        repeat = 5
      )
    )

    val mcts = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "MCTS",
        depth = 5,
        samples = 100,
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

    val experiment = scala.collection.immutable.Seq(
      localSearch,
      //      randomGraph,
      //      mcts,
      //      exhaustive,
    )

    val executor = ExecutorConfig(
      name = "AutoTuning",
      iterations = 11, // execution iterations
      threshold = 10, // speedup to cut iterations
      samples = 5, // samples per tuning run
      global_size_limit = 1024,
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "asum",
      output = "/home/jo/development/rise-lang/shine/experiments/exploration/dodekarch/lowering_exploration",
      inputSizes = scala.collection.immutable.Seq(asum.inputSize),
      metaheuristics = Right(experiment),
      executor = executor,
      lowering = lowering,
      strategies = rules, // is this ignored here?
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

  //
  //  ignore("mm more complex") {
  //
  //    val exhaustive = scala.collection.immutable.Seq(
  //      MetaheuristicConfig(
  //        heuristic = "Exhaustive",
  //        depth = 1000, // is this ignored? -> maybe not for tree window slide
  //        samples = 10, // 7 hours for 1000 samples // 18 hours 5000
  //        repeat = 1
  //      )
  //    )
  //
  //    val experiment = scala.collection.immutable.Seq(
  //      exhaustive,
  //    )
  //
  //    val executor = ExecutorConfig(
  //      name = "AutoTuning",
  //      iterations = 11, // execution iterations
  //      threshold = 10, // speedup to cut iterations
  //      samples = 25, // samples per tuning run
  //      global_size_limit = 1024,
  //    )
  //
  //    // setup explorer config
  //    val explorer = exploration.Explorer(
  //      name = "mm_amd",
  //      output = "/home/jo/development/rise-lang/shine/experiments/exploration/dodekarch/lowering_exploration",
  //      inputSizes = scala.collection.immutable.Seq(mm.N),
  //      metaheuristics = Right(experiment),
  //      executor = executor,
  //      lowering = lowering,
  //      strategies = rules2, // is this ignored here?
  //      hostCode = Some(mm.hostCode),
  //      neighborhoodConfig = NeighborhoodConfig(neighborhood = NGraphChoice),
  //      rewriteFunction = None,
  //      normalForm = None,
  //      importExport = None,
  //      expert = Some(2.3),
  //      default = Some(21500.000),
  //      overwrite = false
  //    )
  //
  //    val result = exploration.explore(explorer)(mm.expressionAMD)
  //
  //  }
  //


  // define acoustic stencil experiment here
  ignore("acoustic stencil") {


    val mcts = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "MCTS",
        depth = 5,
        samples = 1000,
        repeat = 5
      )
    )

    val randomGraph = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "RandomGraph",
        depth = 5,
        samples = 1000,
        repeat = 1
      )
    )

    val random = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Random",
        depth = 5,
        samples = 1000,
        repeat = 1
      )
    )

    val ts = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "TabuSearch",
        depth = 5,
        samples = 1000,
        repeat = 1
      )
    )

    val ts_plain = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "TabuSearchPlain",
        depth = 5,
        samples = 1000,
        repeat = 1
      )
    )

    val exhaustive = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Exhaustive",
        depth = 100, // is this ignored? -> maybe not for tree window slide
        samples = 10000, // 7 hours for 1000 samples // 18 hours 5000
        repeat = 1
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

    val experiment = scala.collection.immutable.Seq(
      //      ts,
      //      localSearch,
      //      ts_plain,
      //      random,
      //      ts,
      //      randomGraph,
      exhaustive
    )

    val executor = ExecutorConfig(
      name = "AutoTuning",
      iterations = 21, // execution iterations
      threshold = 100, // speedup to cut iterations
      samples = 10, // samples per tuning run
      global_size_limit = 1024,
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "acoustic",
      output = "/home/jo/development/rise-lang/shine/experiments/exploration/dodekarch/lowering_exploration/acoustic",
      inputSizes = scala.collection.immutable.Seq(acoustic.N, acoustic.M, acoustic.O), // check how this is used
      metaheuristics = Right(experiment),
      executor = executor,
      lowering = lowering,
      strategies = rules_bidirectional, // is this ignored here?
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