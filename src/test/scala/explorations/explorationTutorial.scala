package explorations

import rise.core.DSL.{fun, lf32}
import rise.core.primitives._
import rise.core.types.DataType.{ArrayType, DataTypeIdentifier, f32}
import rise.elevate.rules.lowering.lowerToC
import rise.elevate.strategies.traversal.everywhere
import rise.elevate.rules.algorithmic.fuseReduceMap
import elevate.core._
import elevate.heuristic_search.Metaheuristic
import elevate.heuristic_search.util.{Solution, hashSolution}
import rise.autotune.HostCode
import rise.core.equality.{exprAlphaEq, typeAlphaEq, typeErasure}
import rise.core.{App, DepApp, DepLambda, Expr, Identifier, Lambda, Literal, Opaque, Primitive, TypeAnnotation, TypeAssertion}
import rise.elevate.Rise
import rise.elevate.rules.traversal.alternative
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.normalForm.DFNF
import rise.elevate.strategies.tiling.tile
import rise.elevate.strategies.traversal._

import java.io.{File, FileOutputStream, PrintWriter}
import java.io.{File, FileInputStream, FileReader}
import rise.core.DSL.ToBeTyped
import rise.core.Expr

import scala.reflect.runtime._
import scala.tools.reflect.ToolBox

import exploration._


object explorationTutorial {
  // see: docs/exploration/tutorial.md
  // input size
  val N = 1 << 9

  // define matrix-matrix multiplication in RISE
  val mm =
    fun(ArrayType(N, ArrayType(N, f32)))(a =>
      fun(ArrayType(N, ArrayType(N, f32)))(b =>
        a |> map(fun(ak =>
          b |> transpose |> map(fun(bk =>
            zip(ak)(bk) |>
              map(fun(x => fst(x) * snd(x))) |>
              reduce(add)(lf32(0.0f))))))))

  // fuse reduce and map
  //  val mmsFused = (`map >> reduce -> reduce` `@` everywhere)(mm).get

  //  val lowering = fuseReduceMap `@` everywhere `;` lowerToC
  //  val lowering = lowerToC


  // dry run

  private val BENF = rise.elevate.strategies.normalForm.BENF()(alternative.RiseTraversable)


  def main(args: Array[String]): Unit = {

    // todo think about of applying normal form
    // todo think about checking
    // todo think about how to get this into rewriting via heuristics
    // e.g. pass function how to create expand strategy


    //
    //    val mm_tiled = expandStrategy.apply(mm_normal)
    //
    //    println("mm_tiled variants: " + mm_tiled.size)
    //
    //    println("mm: \n")
    //    mm_tiled.foreach(elem => println(DFNF().apply(elem).get))
    //
    //    println("\n\n")
    //    val mm_conservative = blockingExploration.blocking_step1.apply(mm).get
    //    println("mm_convervative: \n" + mm_conservative)

    // run exploration with iterative improvement
    //    riseExploration(mm, defaultStrategies.lowering, defaultStrategies.strategies, "exploration/configuration/mm_example_iterative_improvement.json")

    // heuristics

    // todo update exhaustive to tree structure?

    // todo make blocking parameter generic

    // todo add default case, if no tuning parameter was injected? Just tune fake parameter?

    //    riseExploration(
    //      mm,
    //      blockingExploration.lowering,
    //      blockingExploration.strategies,
    //      "exploration/configuration/mm/mm_example_exhaustive.json",
    //      rewriteFunction = Some(rewriteFunction),
    //      afterRewrite = Some(DFNF())
    //    )

    //    riseExploration(
    //      mm,
    //      blockingExploration.lowering,
    //      blockingExploration.strategies,
    //      "exploration/configuration/mm/mm_example_cot.json",
    //      rewriteFunction = Some(rewriteFunction),
    //      afterRewrite = Some(DFNF())
    //    )

    //
    //    riseExploration(
    //      mm,
    //      blockingExploration.lowering,
    //      blockingExploration.strategies,
    //      "exploration/configuration/mm/mm_example_autotuner_c.json",
    //      rewriteFunction = Some(rewriteFunction),
    //      afterRewrite = Some(DFNF())
    //    )

    //    riseExploration(
    //      mm,
    //      blockingExploration.lowering,
    //      blockingExploration.strategies,
    //      "exploration/configuration/mm/mm_example_autotuner_c.json",
    //      rewriteFunction = Some(rewriter.everywhere.rewriteFunction),
    //      afterRewrite = Some(DFNF()),
    //    importExport = Some(importSolution, exportSolution)
    //    )

    val metaheuristics = Seq(
      MetaheuristicConfig(heuristic = "autotuner",
        depth = 3,
        iteration = 1
      )
    )

    val executor = ExecutorConfig(
      name = "C",
      iterations = 1,
      threshold = 1000
    )

    // setup explorer config
    val explorer = exploration.Explorer(
      name = "mm_example_autotuner",
      output = "exploration",
      inputSize = 512,
      metaheuristics = metaheuristics,
      executor = executor,
      lowering = exploration.strategies.blockingExploration.lowering,
      strategies = exploration.strategies.blockingExploration.strategies,
      rewriteFunction = Some(exploration.rewriter.everywhere.rewriteFunction),
      normalForm = Some(DFNF()),
      importExport = Some(exploration.explorationUtil.IO.importExport)
    )

    val explorationResult = exploration.explore(explorer)(mm)


    //    riseExploration(mm, blockingExploration.lowering, blockingExploration.strategies, "exploration/configuration/mm/mm_example_iterative_improvement.json")
    //        riseExploration(mm, blockingExploration.lowering, blockingExploration.strategies, "exploration/configuration/mm/mm_example_exhaustive.json")
    //            riseExploration(mm, blockingExploration.lowering, blockingExploration.strategies, "exploration/configuration/mm/mm_example_random_sampling.json")
    //    riseExploration(mm, blockingExploration.lowering, blockingExploration.strategies, "exploration/configuration/mm/mm_example_random.json")

    // hm index

    // cot

    //    riseExploration(mm, lowering, defaultStrategies.strategies, "exploration/configuration/mm_example_random.json")

    // find results in exploration/ folder
  }

}

