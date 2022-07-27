package exploration

import strategies.{blockingExploration, defaultStrategies}
import rise.core.DSL.{fun, lf32}
import rise.core.primitives._
import rise.core.types.DataType.{ArrayType, DataTypeIdentifier, f32}
import rise.elevate.rules.lowering.lowerToC
import rise.elevate.strategies.traversal.everywhere
import rise.elevate.rules.algorithmic.fuseReduceMap
import elevate.core._
import elevate.heuristic_search.util.Solution
import rise.core.equality.{exprAlphaEq, typeAlphaEq, typeErasure}
import rise.core.{App, DepApp, DepLambda, Expr, Identifier, Lambda, Literal, Opaque, Primitive, TypeAnnotation, TypeAssertion}
import rise.elevate.Rise
import rise.elevate.rules.traversal.alternative
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.normalForm.DFNF
import rise.elevate.strategies.tiling.tile
import rise.elevate.strategies.traversal._

class explorationTutorial extends test_util.Tests {
  // see: docs/exploration/tutorial.md
  // input size
  val N = 1 << 9

  // define matrix-matrix multiplication in RISE
  val mm: Rise =
    fun(ArrayType(N, ArrayType(N, f32)))(a =>
      fun(ArrayType(N, ArrayType(N, f32)))(b =>
        a |> map(fun(ak =>
          b |> transpose |> map(fun(bk =>
            zip(ak)(bk) |>
              map(fun(x => fst(x) * snd(x))) |>
              reduce(add)(lf32(0.0f))))))))

  test("run expert configuration") {
    // expert
    val mmExpert = blockingExploration.expert.apply(mm).get
    val gold = lowerToC.apply(apps.tvmGemm.par.apply(mm).get).get

    val executor = exploration.runner.CExecutor(
      lowering = blockingExploration.lowering,
      output = "exploration",
      iterations = 10,
      goldExpression = gold,
      inputSize = N,
      saveToDisk = false,
      timeout = 10000,
      threshold = 100,
    )

    val result = executor.execute(Solution[Rise](mmExpert, scala.collection.immutable.Seq(blockingExploration.expert)))

    println("result: " + result)
  }

  test("run default configuration") {
    // expert
    val mmDefault = mm
    val gold = lowerToC.apply(apps.tvmGemm.par.apply(mm).get).get

    val executor = exploration.runner.CExecutor(
      lowering = blockingExploration.lowering,
      output = "exploration",
      iterations = 10,
      goldExpression = gold,
      inputSize = N,
      saveToDisk = false,
      timeout = 100000,
      threshold = 100
    )

    val result = executor.execute(Solution[Rise](mmDefault, scala.collection.immutable.Seq(blockingExploration.expert)))

    println("result: " + result)

  }

  test("run exploration") {

    val autotuner = scala.collection.immutable.Seq(
      MetaheuristicConfig(
        heuristic = "autotuner",
        depth = 3,
        samples = 200, // ignored ?
      )
    )

    val experiment = scala.collection.immutable.Seq(
      autotuner
    )

    val executor = ExecutorConfig(
      name = "C",
      iterations = 11,
      threshold = 10
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "mmCPU_parallel_fine_light",
      output = "/home/jo/development/experiments/exploration/thinkjo/autotuner", // adjust this
      inputSize = N,
      metaheuristics = Right(experiment),
      executor = executor,
      lowering = exploration.strategies.blockingExploration.lowering,
      strategies = blockingExploration.par,
      rewriteFunction = Some(exploration.rewriter.everywhere.rewriteFunction(blockingExploration.par)),
      normalForm = Some(DFNF()),
      importExport = Some(exploration.explorationUtil.IO.importExport),
      expert = Some(1.931617), // adjust this
      overwrite = false // ignore
    )

    // repeat this
    val explorationResult = exploration.explore(explorer)(mm)

  }



  //  def main(args: Array[String]): Unit = {
  //
  //    runExpert()

  // Strategies
  //    riseExploration(
  //      mm,
  //      blockingExploration.lowering,
  //      blockingExploration.strategies,
  //      "exploration/configuration/mm/mm_example_autotuner.json",
  //      rewriteFunction = None,
  //      afterRewrite = None
  //    )


  // Rules
  //    riseExploration(
  //      mm,
  //      blockingExploration.lowering,
  //      blockingExploration.strategies, // is ignored here
  //      "exploration/configuration/mm/mm_example_autotuner.json",
  //      rewriteFunction = Some(rewriteFunction),
  //      afterRewrite = Some(DFNF())
  //    )

  // Rules with Parameters
  // todo change rules here
  //    riseExploration(
  //      mm,
  //      blockingExploration.lowering,
  //      blockingExploration.strategies, // is ignored here
  //      "exploration/configuration/mm/mm_example_autotuner.json",
  //      rewriteFunction = Some(rewriteFunction),
  //      afterRewrite = Some(DFNF())
  //    )
  //  }
}

