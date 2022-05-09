package apps.autotuning

import apps.gemv.ocl._
import apps.mm.dot
import apps.separableConvolution2D.mulT
import apps.tvmGemm.{innermost, outermost, par}
import arithexpr.arithmetic.RangeMul
import rise.autotune
import rise.autotune._
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.types._
import shine.OpenCL.{GlobalSize, LocalSize}
import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import exploration.runner.CExecutor
import rise.core.DSL._
import rise.core.primitives._
import rise.core.types.DataType._
import rise.elevate._
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.lowering._
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.algorithmic.reorder
import rise.elevate.strategies.lowering._
import rise.elevate.strategies.normalForm._
import rise.elevate.strategies.predicate._
import rise.elevate.strategies.tiling._
import rise.elevate.strategies.traversal
import rise.elevate.strategies.traversal._
import elevate.heuristic_search.util.{IOHelper, Solution}

import scala.collection.immutable

class mmTVMTuning extends test_util.Tests {

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

  // -- LOOP PERMUTATION -------------------------------------------------------

  val loopPerm: Strategy[Rise] = baseline `;`
    (tile() `@` outermost(mapNest(2))) `;;`
    (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
    (tunable("split", splitStrategy) `@` innermost(isFullyAppliedReduce)) `;;`
    reorder(List(1, 2, 5, 3, 6, 4)) `;;`
    (tunable("vec", vectorize) `@` innermost(isFullyAppliedMap))

  val mm2: Rise = loopPerm.apply(mm).get

  val mm_par = par.apply(mm)
  val gold = lowerToC.apply(mm_par.get).get

  // yo
  //  mm2

  // use CExecutor?

  // how to inject tps?


  test("test tvm gemm") {

    val params = autotune.constraints.collectParameters(mm2)
    val constr = autotune.constraints.collectConstraints(mm2, params)

    println("mm: \n" + mm2)
    println("params: " + params)
    println("constr: " + constr.mkString(", "))

    // inject parameters
    val params2: Map[Nat, Nat] = Map(
      TuningParameter("tuned_tile51") -> (32: Nat),
      TuningParameter("tuned_tile52") -> (32: Nat),
      TuningParameter("tuned_vec101") -> (32: Nat),
      TuningParameter("tuned_split100") -> (4: Nat),
    )

    // problem here: Replacement of tuning params does not work
    val eSub = rise.core.substitute.natsInExpr(params2, mm2)

    // execute
    // problem2: Permutation variable

    println("eSub: \n" + eSub)

    val executor = CExecutor(
      lowering = lowerToC,
      goldExpression = gold,
      iterations = 10,
      inputSize = 1024,
      threshold = 1000,
      output = "autotuning"
    )

    val strategies = immutable.Seq.empty[Strategy[Rise]]

    println("now execute")
    val result = executor.execute(Solution(eSub, strategies))._2


    println("result: " + result)
  }

  def executeMM(e: Expr, s0: Nat) = {

    //    val result = autotune.execution.execute(
    //      expression = eSub,
    //      hostCode = HostCode(init(128, 128), compute, finish),
    //      timeouts = Timeouts(5000, 5000, 5000),
    //      executionIterations = 100,
    //      speedupFactor = 100,
    //      execution = Median
    //    )
    //    println("result: " + result.runtime)

  }

  //  test("exeute gemv version") {
  //    executeGemv(gemvBlastNTuning, 64)
  //    executeGemv(gemvBlastTTuning, 64)
  //    executeGemv(gemvFusedTuning, 64) // ignore s0 in this case
  //    executeGemv(gemvFusedAMDTuning, 128)
  //    executeGemv(gemvKeplerBestTuning, 128)
  //  }
  //
  //  test("tune gemv version") {
  //    runTuning(gemvBlastNTuning)
  //    runTuning(gemvBlastTTuning)
  //    runTuning(gemvFusedTuning) // ignore s0 in this case
  //    runTuning(gemvFusedAMDTuning)
  //    runTuning(gemvKeplerBestTuning)
  //  }


  def runTuning(e: Expr) = {
    //    //    val version = autotuning.parseName(configFile)
    //
    //    val tuner = Tuner(
    //      hostCode = HostCode(init(1024, 1024), compute, finish),
    //      inputSizes = Seq(1024, 1024, 1024),
    //      samples = 20,
    //      name = "gemv",
    //      output = s"autotuning/gemv",
    //      timeouts = Timeouts(10000, 10000, 10000),
    //      executionIterations = 10,
    //      speedupFactor = 100,
    //      configFile = None,
    //      hmConstraints = true,
    //      runtimeStatistic = Minimum,
    //      saveToFile = true
    //    )
    //
    //    autotune.search(tuner)(e)
  }
}