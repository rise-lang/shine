package explorations

import apps.tvmGemm
import apps.tvmGemm.{innermost, isFullyAppliedReduce}
import exploration.strategies.blockingExploration
import rise.autotune
import rise.autotune.{AutoTuningError, EXECUTION_ERROR, HostCode, Timeouts}
import rise.core.Expr
import rise.core.primitives.map
import rise.elevate.strategies.normalForm.DFNF
import elevate.core._
import exploration.runner.{CExecutor}
import rise.core.DSL.{fun, lf32}
import rise.core.primitives.{add, fst, map, reduce, snd, transpose, zip}
import rise.core.types.DataType.{ArrayType, f32}
import rise.elevate.{NormalizedThen, Rise, tunable}
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.lowering._
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.algorithmic.reorder

import rise.elevate.strategies.predicate._
import rise.elevate.strategies.tiling._
import rise.elevate.strategies.traversal
import rise.elevate.strategies.traversal._
import rise.autotune._

class rewritingTuningTradeOff extends test_util.Tests {

  // helper
  val outermost: (Strategy[Rise]) => (Strategy[Rise]) => Strategy[Rise] =
    traversal.outermost(alternative2.RiseTraversable)

  // define mm baseline
  val N = 128

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

  // define lowering
  val lowering: Strategy[Rise] = blockingExploration.lowering

  // define gold
  val gold: Rise = lowering.apply(tvmGemm.par.apply(mm).get).get

  // stage 0 (baseline)
  val stage0: Strategy[Rise] = elevate.core.strategies.basic.id[Rise]

  // stage 1 (fused)
  val stage1: Strategy[Rise] = fuseReduceMap `@` everywhere

  // stage 2 (tiled but not reorderd)
  val stage2_default: Strategy[Rise] = (tile(32, 32) `@` outermost(mapNest(2))) `;` DFNF()
  val stage2_worst: Strategy[Rise] = (tile(1, 1) `@` outermost(mapNest(2))) `;` DFNF()
  val stage2_tuning: Strategy[Rise] = (tile() `@` outermost(mapNest(2))) `;` DFNF()

  // stage 3 (fissioned)
  val stage3: Strategy[Rise] = (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;` DFNF()

  // stage 4 (reordered)
  val stage4_default: Strategy[Rise] = (splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;` reorder(List(1, 2, 5, 6, 3, 4))
  // todo adjust this 4 here
  val stage4_worst: Strategy[Rise] = (splitStrategy(4) `@` innermost(isFullyAppliedReduce)) `;;` reorder(List(1, 2, 5, 6, 3, 4))
  val stage4_tuning: Strategy[Rise] = (tunable(splitStrategy) `@` innermost(isFullyAppliedReduce)) `;;` reorder(List(1, 2, 5, 6, 3, 4))

  // stage 5 (vectorized)
  val stage5_default: Strategy[Rise] = (vectorize(32) `@` innermost(isApplied(isApplied(isMap))))
  val stage5_worst: Strategy[Rise] = (vectorize(2) `@` innermost(isApplied(isApplied(isMap))))
  val stage5_tuning: Strategy[Rise] = (tunable(vectorize) `@` innermost(isApplied(isApplied(isMap))))


  // stage 6 (parallel)
  val stage6: Strategy[Rise] = (parallel() `@` outermost(isApplied(isMap))) `;;` (unroll `@` innermost(isReduceSeq))

  // define global executor
  val executor = CExecutor(
    lowering = lowering,
    goldExpression = gold,
    iterations = 5,
    inputSize = 128,
    threshold = 100,
    output = "exploration",
    saveToDisk = false
  )

  val execute: Expr => (
    Either[AutoTuningError, Double],
      Option[Double],
      Option[Double],
      Option[Double]
    ) = e => {

    val executionStart = System.currentTimeMillis()

    val result = executor.execute(e)

    // todo move to other thing
    val runtime: Either[AutoTuningError, Double] = result match {
      case Some(value) => Right(value)
      case None => Left(AutoTuningError(EXECUTION_ERROR, None))
    }

    // todo measure these properly
    val codegenTime = (System.currentTimeMillis() - executionStart).toDouble
    val compilationTime = (System.currentTimeMillis() - executionStart).toDouble
    val executionTime = (System.currentTimeMillis() - executionStart).toDouble

    (runtime,
      Some(codegenTime),
      Some(compilationTime),
      Some(executionTime))
  }

  // test to execute all of them
  def stage(stages: scala.collection.immutable.Seq[Strategy[Rise]], stage: Int) = {
    println("stage: " + stage)

    // rewrite to stage
    val stagedExpression: Rise = lowering.apply(
      stages
        .take(stage + 1)
        .reduceLeft((a, b) => a `;` b)
        .apply(mm).get
    ).get

    val params = autotune.constraints.collectParameters(stagedExpression)

    params.size match {
      case 0 => // CExecutor

        val result = executor.execute(stagedExpression)
        println("result: " + result)

        result

      case _ => // Auto Tuning

        val tuner = Tuner(
          hostCode = HostCode("", "", ""),
          inputSizes = scala.collection.immutable.Seq(N),
          samples = 20, // defined by config file, value is ignored
          name = s"stage${stage}",
          output = s"exploration",
          timeouts = Timeouts(10000, 10000, 10000),
          executionIterations = 10,
          speedupFactor = 100,
          configFile = None,
          hmConstraints = true,
          runtimeStatistic = Median,
          executor = Some(execute),
          saveToFile = false,
          disableChecking = true
        )

        val result = autotune.search(tuner)(stagedExpression)

        autotune.getBest(result.samples) match {
          case Some(value) => value.runtime match {
            case Left(error) => None
            case Right(runtime) => Some(runtime.value)
          }
          case None => None
        }
    }
  }

  val stages_default = scala.collection.immutable.Seq(
    stage0,
    stage1,
    stage2_default,
    stage3,
    stage4_default,
    stage5_default,
    stage6
  )

  val stages_worst = scala.collection.immutable.Seq(
    stage0,
    stage1,
    stage2_worst,
    stage3,
    stage4_worst,
    stage5_worst,
    stage6
  )

  val stages_tuning = scala.collection.immutable.Seq(
    stage0,
    stage1,
    stage2_tuning,
    stage3,
    stage4_tuning,
    stage5_tuning,
    stage6
  )

  test("test rewriting and tuning") {
    val result_default = Range(0, 7).map(i => stage(stages_default, i))
    val result_worst = Range(0, 7).map(i => stage(stages_worst, i))
    val result_tuning = Range(0, 7).map(i => stage(stages_tuning, i))

    println("\n\nResults: ")

    println("Default: ")
    Range(0, 7).zip(result_default).foreach(elem => println(s"[Stage ${elem._1}] : ${elem._2}"))

    println()

    println("Worst: ")
    Range(0, 7).zip(result_worst).foreach(elem => println(s"[Stage ${elem._1}] : ${elem._2}"))

    println()

    println("Tuning: ")
    Range(0, 7).zip(result_tuning).foreach(elem => println(s"[Stage ${elem._1}] : ${elem._2}"))

  }


}
