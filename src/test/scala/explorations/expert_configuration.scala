package explorations

import apps.tvmGemm.{innermost, outermost}
import arithexpr.arithmetic.RangeMul
import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.heuristic_search.util.{RewriteIdentifier, Solution}
import elevate.macros.RuleMacro.rule
import exploration.neighborhoods._
import exploration.runner.{AutoTuningExecutor, CExecutor}
import exploration.{ExecutorConfig, MetaheuristicConfig, NeighborhoodConfig, rewrite_and_execute}
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

class expert_configuration extends test_util.Tests {

  // rules
  @rule def splitJoinRule: Strategy[Rise] = tunable(splitJoin)

  // lowering

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
      lowering3 `;` // map -> map global 0 (topdown/outermost)
      lowering4 `;` // map -> mapGlobal 1 (topdown/outermost)
      lowering5 // map (compute) -> mapSeq
    //      lowering6
  }


  test("mm") {

    // rewrite
    val rewrites = scala.collection.immutable.Seq(
      //
      //      RewriteIdentifier[Rise](
      //        strategy = rise.elevate.rules.lowering.mapGlobal(1),
      //        location = 0
      //      ),
      RewriteIdentifier[Rise](
        strategy = fuseReduceMap,
        location = 0
      )
    )

    val executor = ExecutorConfig(
      name = "AutoTuning",
      iterations = 51, // execution iterations
      threshold = 10, // speedup to cut iterations
      samples = 10, // samples per tuning run
      global_size_limit = 1024,
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "mm",
      output = "/home/jo/shine/experiments/exploration/expert",
      inputSizes = scala.collection.immutable.Seq(mm.N),
      metaheuristics = Right(null),
      executor = executor,
      lowering = lowering,
      strategies = null, // is this ignored here?
      hostCode = Some(mm.hostCode),
      neighborhoodConfig = NeighborhoodConfig(neighborhood = NGraphChoice),
      rewriteFunction = None,
      normalForm = None,
      importExport = None,
      expert = None,
      default = None,
      overwrite = false
    )

    rewrite_and_execute(
      expression = mm.expression,
      rewrites = rewrites,
      explorer = explorer
    )

  }


  test("acoustic rewrite") {

    //    rewrite
    val rewrites = scala.collection.immutable.Seq(

    )

    val executor = ExecutorConfig(
      name = "AutoTuning",
      iterations = 51, // execution iterations
      threshold = 10, // speedup to cut iterations
      samples = 10, // samples per tuning run
      global_size_limit = 1024,
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "acoustic",
      output = "/home/jo/shine/experiments/exploration/expert",
      inputSizes = scala.collection.immutable.Seq(acoustic.N, acoustic.M, acoustic.O), // check how this is used
      metaheuristics = Right(null),
      executor = executor,
      lowering = lowering,
      strategies = null, // is this ignored here?
      hostCode = Some(acoustic.hostCode),
      neighborhoodConfig = NeighborhoodConfig(neighborhood = NGraphChoice),
      rewriteFunction = None,
      normalForm = None,
      importExport = None,
      expert = None,
      default = None,
      overwrite = false
    )

    rewrite_and_execute(
      expression = acoustic.expression,
      rewrites = rewrites,
      explorer = explorer
    )
  }
}
