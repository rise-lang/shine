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


  def main(args: Array[String]): Unit = {

    val metaheuristics = Seq(
      MetaheuristicConfig(
        heuristic = "autotuner",
        depth = 4,
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
      output = "/home/jo/development/experiments/exploration/thinkjo/",
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

    println("explorationResult: " + explorationResult)
  }

}

