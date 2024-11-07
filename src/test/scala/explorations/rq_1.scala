package explorations

import arithexpr.arithmetic.RangeMul
import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.macros.RuleMacro.rule
import exploration.neighborhoods._
import exploration.{ExecutorConfig, MetaheuristicConfig, NeighborhoodConfig}
import rise.autotune
import rise.autotune.{HostCode, Median, Minimum, Timeouts, Tuner, tuningParam, wrapOclRun}
import rise.core.DSL._
import rise.core.Expr
import rise.core.primitives.{let => _}
import rise.core.types.{Nat, TuningParameter}
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.lowering._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.traversal._
import rise.elevate.{Rise, tunable}
import shine.OpenCL.{GlobalSize, LocalSize}

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

  def run_scal() = {

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


  def run_kmeans() = {

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


  def run_mm() = {

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


  def run_asum() = {

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
  def run_acoustic() = {

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

  def run_kmeans_expert() = {
    ////    val params: Map[Nat, Nat] = Map(
    ////      TuningParameter("ls0") -> (32: Nat),
    ////      TuningParameter("ls1") -> (1: Nat),
    ////      TuningParameter("gs0") -> (1024: Nat),
    ////      TuningParameter("gs1") -> (1: Nat)
    ////    )
    //
    //    val kmeans_replaced = rise.core.substitute.natsInExpr(params, expression)
    //
    //    inputSizes = scala.collection.immutable.Seq(kmeans.p, kmeans.c, kmeans.f),
    //    val tuner = Tuner(
    //      hostCode = kmeans.hostCode,
    //      inputSizes = scala.collection.immutable.Seq(kmeans.p, kmeans.c, kmeans.f),
    //      samples = 20,
    //      name = "kmeans",
    //      output = "autotuning/kmeans",
    //      timeouts = Timeouts(10000, 10000, 10000),
    //      executionIterations = 10,
    //      speedupFactor = 100,
    //      configFile = Some("autotuning/config/kmeans/kmeans_exhaustive.json"),
    //      //      configFile = None,
    //      hmConstraints = true,
    //      runtimeStatistic = Median
    //    )
    //
    //    val tuningResult = autotune.search(tuner)(expression)

    val expression: Expr = wrapOclRun(LocalSize(32, 32), GlobalSize(1024, 1024))(kmeans.expert)

    val result = autotune.execution.execute(
      expression = expression,
      hostCode = kmeans.hostCode,
      timeouts = Timeouts(30000, 30000, 30000),
      executionIterations = 51,
      speedupFactor = 100,
      execution = Median
    )

    println("kmeans: " + result)
  }

  def run_acoustic_expert() = {
    val expression: Expr = wrapOclRun(LocalSize(32, 8), GlobalSize(256, 128))(acoustic.expert)

    val result = autotune.execution.execute(
      expression = expression,
      hostCode = acoustic.hostCode,
      timeouts = Timeouts(30000, 30000, 30000),
      executionIterations = 51,
      speedupFactor = 100,
      execution = Median
    )

    println("acoustic: " + result)
  }

  def run_asum_expert() = {

    val params: Map[Nat, Nat] = Map(
      TuningParameter("sp0") -> ((2048 * 128): Nat),
      TuningParameter("sp1") -> (2048: Nat),
      TuningParameter("stride") -> (128: Nat),
    )

    val asum_expert: Expr = rise.core.substitute.natsInExpr(params, asum.expert)

    val expression: Expr = wrapOclRun(LocalSize(128), GlobalSize(1024, 1))(asum_expert)

    val result = autotune.execution.execute(
      expression = expression,
      hostCode = asum.hostCode,
      timeouts = Timeouts(30000, 30000, 30000),
      executionIterations = 51,
      speedupFactor = 100,
      execution = Median
    )

    println("asum: " + result)
  }

  def run_scal_expert() = {

    val expression: Expr = wrapOclRun(LocalSize(256), GlobalSize(1024))(scal.expert)

    val result = autotune.execution.execute(
      expression = expression,
      hostCode = scal.hostCode,
      timeouts = Timeouts(30000, 30000, 30000),
      executionIterations = 51,
      speedupFactor = 100,
      execution = Median
    )

    println("scal: " + result)

  }

  def run_mm_expert() = {
    //    val scal_expert: Expr = rise.core.substitute.natsInExpr(params, scal.expert)

    //    println("test")
    val fused = (fuseReduceMap `@` topDown[Rise]).apply(mm.expression).get
    //    println("fused")
    val p0 = (mapGlobal(0) `@` topDown[Rise]).apply(fused).get
    //    println("parallel")
    val p1 = (mapGlobal(1) `@` topDown[Rise]).apply(p0).get
    //    println("parallel2")
    val re = (reduceOCL() `@` topDown[Rise]).apply(p1).get
    //    println("reduce")

    val expression: Expr = wrapOclRun(LocalSize(32, 32), GlobalSize(1024, 1024))(re)

    val result = autotune.execution.execute(
      expression = expression,
      hostCode = mm.hostCode,
      timeouts = Timeouts(30000, 30000, 30000),
      executionIterations = 51,
      speedupFactor = 100,
      execution = Median
    )

    println("MM: " + result)
  }

  test("expert config") {
    run_kmeans_expert()
    run_acoustic_expert()
    run_asum_expert()
    run_scal_expert()
    run_mm_expert()
    //        run_mm_2070_expert()
  }

  //  test("acoustic") {
  //    run_acoustic()
  //  }
  //
  //  test("asum") {
  //    run_asum()
  //  }
  //
  //  test("kmeans") {
  //    run_kmeans()
  //  }
  //
  //  test("mm") {
  //    run_mm()
  //  }
  //
  //  test("scal") {
  //    run_scal()
  //  }
  //
}