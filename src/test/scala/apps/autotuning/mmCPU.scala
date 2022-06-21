package apps.autotuning

import apps.tvmGemm.{innermost, outermost, par}
import elevate.core._
import elevate.core.strategies.traversal._
import elevate.heuristic_search.util.Solution
import rise.autotune
import rise.autotune._
import rise.core.DSL._
import rise.core._
import rise.core.primitives._
import rise.core.types.DataType._
import rise.elevate._
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.lowering._
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.algorithmic.reorder
import rise.elevate.strategies.normalForm._
import rise.elevate.strategies.predicate._
import rise.elevate.strategies.tiling._
import rise.elevate.strategies.traversal._

import scala.collection.immutable
import scala.language.postfixOps
import exploration.runner.CExecutor


class mmCPU extends test_util.Tests {

  // tvm gemm
  val N = 1024

  // todo check input vars, maybe dep fun
  val mm: Expr = //infer(
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

  // -- BASELINE ---------------------------------------------------------------

  val baseline: Strategy[Rise] = DFNF()(default.RiseTraversable) `;`
    (fuseReduceMap `@` topDown[Rise])

  // -- BLOCKING ---------------------------------------------------------------

  val isFullyAppliedReduce: Strategy[Rise] = isApplied(isApplied(isApplied(isReduce)))

  val isFullyAppliedMap: Strategy[Rise] = isApplied(isApplied(isMap))

  //  // -- LOOP PERMUTATION -------------------------------------------------------
  //
  //  val loopPerm: Strategy[Rise] = baseline `;`
  //    (tile() `@` outermost(mapNest(2))) `;;`
  //    (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
  //    (tunable("split", splitStrategy) `@` innermost(isFullyAppliedReduce)) `;;`
  //    reorder(List(1, 2, 5, 3, 6, 4)) `;;`
  //    (tunable("vec", vectorize) `@` innermost(isFullyAppliedMap))
  //
  //  val mm2: Rise = loopPerm.apply(mm).get

  val mm_par = par.apply(mm)
  val gold = lowerToC.apply(mm_par.get).get

  val inject: (Expr, Map[String, Int], Map[String, List[Int]]) => Either[String, Expr] = (e, tuningParameterMap, permutationMap) => {

    val tileX = tuningParameterMap.get("tuned_tile51").get
    val tileY = tuningParameterMap.get("tuned_tile52").get
    val split = tuningParameterMap.get("tuned_split100").get
    val reordering = permutationMap.get("tuned_reorder").get.map(x => x + 1)
    val vec = tuningParameterMap.get("tuned_vec101").get

    //    val tileX = 32
    //    val tileY = 32
    //    val split = 4
    //    val vec = 32

    val loopPerm: Strategy[Rise] = baseline `;`
      (tile(tileX, tileY) `@` outermost(mapNest(2))) `;;`
      (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
      (splitStrategy(split) `@` innermost(isFullyAppliedReduce)) `;;`
      reorder(reordering) `;;`
      (vectorize(vec) `@` innermost(isFullyAppliedMap))

    val result = try {
      loopPerm.apply(e)
    } catch {
      case e: Throwable => {
        println("e: " + e)
        Failure(loopPerm)
      }
    }

    result match {
      case Success(expression) => Right(expression)
      case Failure(value) => Left(value.toString())
    }
  }

  val executor = CExecutor(
    lowering = lowerToC,
    goldExpression = gold,
    iterations = 10,
    inputSize = 1024,
    threshold = 10,
    output = "autotuning",
    saveToDisk = false
  )


  val execute: Expr => (
    Either[AutoTuningError, Double],
      Option[Double],
      Option[Double],
      Option[Double]
    ) = e => {

    val strategies = immutable.Seq.empty[Strategy[Rise]]

    val executionStart = System.currentTimeMillis()
    val result = executor.execute(Solution(e, strategies)).performance

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


  test("run experiment mmCPU") {
    val inputSize: Int = 1024

    val configs = Seq(
      s"autotuning/config/mmCPU_reordering/${inputSize.toString}/rs_cot_${inputSize.toString}.json",
      //      s"autotuning/config/mmCPU_reordering/${inputSize.toString}/rs_emb_${inputSize.toString}.json",
      //      s"autotuning/config/mmCPU_reordering/${inputSize.toString}/ls_cot_${inputSize.toString}.json",
      s"autotuning/config/mmCPU_reordering/${inputSize.toString}/bogp_cot_${inputSize.toString}.json",
      s"autotuning/config/mmCPU_reordering/${inputSize.toString}/bogplsp_cot_${inputSize.toString}.json",
      s"autotuning/config/mmCPU_reordering/${inputSize.toString}/atf_emb_${inputSize.toString}.json",
      //      s"autotuning/config/mmCPU_reordering/${inputSize.toString}/ytopt_${inputSize.toString}.json",
    )

    runExperiment(
      name = s"mmCPU_reordering_${inputSize}",
      configFiles = configs,
      iterations = 5,
      //      s"autotuning/mm_${inputSize}",
      output = s"experiment/results/mmCPU_reordering_${inputSize}",
      mm,
      HostCode("", "", ""),
      Seq(inputSize, inputSize, inputSize),
      strategyMode = Some(inject),
      executor = Some(execute),
      //      plotOnly = true
      plotOnly = false
    )
  }


  ignore("tune tvmgemm example reordering") {

    val tuner = Tuner(
      hostCode = HostCode("", "", ""), // we don't need that
      samples = 100,
      name = "rs_emb_1024",
      output = "autotuning/tvm_gemm_reordering",
      timeouts = Timeouts(5000, 5000, 1000),
      executionIterations = 10,
      speedupFactor = 100,
      //      configFile = Some("autotuning/config/mmCPU_reordering/1024/rs_cot_1024.json"),
      configFile = Some("autotuning/config/mmCPU_reordering/1024/rs_emb_1024.json"),
      //      configFile = Some("autotuning/config/mmCPU/rs_cot_1024_reorder.json"),
      hmConstraints = true,
      strategyMode = Some(inject),
      executor = Some(execute),
      saveToFile = true
    )

    val result = autotune.search(tuner)(mm)

    println("result: " + result)

  }

}