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


class lowering_exploration extends test_util.Tests {


  // @rule broken here? why?
  @rule def splitJoinRule: Strategy[Rise] = tunable(splitJoin)

  //  @rule def mapGlobalRule: Strategy[Rise] = tunable(rise.elevate.rules.lowering.mapGlobal)

  @rule def vectorizeRule: Strategy[Rise] = tunable(rise.elevate.rules.lowering.vectorize)


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
      //      rise.elevate.rules.lowering.oclReduceSeqLocal,
      //      rise.elevate.rules.lowering.oclReduceSeqGlobal,
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

  val lowering: Strategy[Rise] =
    lowering0 `;` // add copies if necessary
      lowering1 `;` // reduce -> reduceSeq
      lowering2 `;` // reduceSeq -> reduceOcl
      //      lowering3 `;` // map -> map global 0 (topdown/outermost)
      //      lowering4 `;` // map -> mapGlobal 1 (topdown/outermost)
      lowering5 `;` // map (compute) -> mapSeq
      lowering6


  //  define mm experiment here
  ignore("mm") {

    val exhaustive = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Exhaustive",
        depth = 1000, // is this ignored? -> maybe not for tree window slide
        samples = 100, // 7 hours for 1000 samples // 18 hours 5000
        repeat = 1
      )
    )

    val experiment = scala.collection.immutable.Seq(
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

    //    mm.generate_and_store_gold(mm.N)

  }

  // define acoustic stencil experiment here
  test("acoustic stencil") {

    val exhaustive = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "Exhaustive",
        depth = 100, // is this ignored? -> maybe not for tree window slide
        samples = 10000, // 7 hours for 1000 samples // 18 hours 5000
        repeat = 1
      )
    )

    val experiment = scala.collection.immutable.Seq(
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
      name = "acoustic",
      output = "/home/jo/development/rise-lang/shine/experiments/exploration/dodekarch/lowering_exploration/acoustic",
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