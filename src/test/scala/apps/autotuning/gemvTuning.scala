package apps.autotuning

import apps.gemv.ocl._
import arithexpr.arithmetic.{ArithExpr, RangeAdd, RangeMul}
import rise.autotune
import rise.autotune._
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.types._
import shine.OpenCL.{GlobalSize, LocalSize}

class gemvTuning extends test_util.Tests {

  // gemvBlastN
  val gemvBlastNTuning: ToBeTyped[Expr] =
    tuningParam("s0", RangeMul(1, 1024, 2), (s0: Nat) =>
      wrapOclGemv(gemvBlastNParam(s0))
    )
  // gemvBlastT
  val gemvBlastTTuning: ToBeTyped[Expr] =
    tuningParam("s0", RangeMul(1, 1024, 2), (s0: Nat) =>
      wrapOclGemv(gemvBlastTParam(s0))
    )
  // gemvFused
  val gemvFusedTuning: ToBeTyped[Expr] =
    wrapOclGemv(gemvFused)

  // gemvFusedAMD
  val gemvFusedAMDTuning: ToBeTyped[Expr] =
    tuningParam("s0", RangeMul(1, 1024, 2), (s0: Nat) =>
      wrapOclGemv(gemvKeplerBestParam(s0))
    )

  // gemvKeplerBest
  val gemvKeplerBestTuning: ToBeTyped[Expr] =
    tuningParam("s0", RangeMul(1, 1024, 2), (s0: Nat) =>
      wrapOclGemv(gemvKeplerBestParam(s0))
    )

  val gemvAMDNoTuning: ToBeTyped[Expr] =
    wrapOclGemv(gemvFusedAMD)

  def wrapOclGemv(e: Expr): Expr = {
    tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
      tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
        tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
          tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
            wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(e)
          ))))
  }

  // scalastyle:off
  val init: (Int, Int) => String = (N, M) => {
    s"""
       |const int N = ${N};
       |const int M = ${M};
       |
       |srand(time(NULL));
       |
       |Buffer inputM = createBuffer(ctx, N * M * sizeof(float), HOST_WRITE | DEVICE_READ);
       |Buffer inputX = createBuffer(ctx, N * sizeof(float), HOST_READ | DEVICE_WRITE);
       |Buffer inputY = createBuffer(ctx, M * sizeof(float), HOST_READ | DEVICE_WRITE);
       |Buffer outputZ = createBuffer(ctx, N * sizeof(float), HOST_READ | DEVICE_WRITE);
       |
       |float* inM = hostBufferSync(ctx, inputM, N * M * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < N * M; i++) {
       |  inM[i] = (float)(rand());
       |}
       |
       |float* inX = hostBufferSync(ctx, inputX, N * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < N; i++) {
       |  inX[i] = (float)(rand());
       |}
       |
       |float* inY = hostBufferSync(ctx, inputY, M * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < M; i++) {
       |  inY[i] = (float)(rand());
       |}
       |
       |int alpha = (float)(rand());
       |int beta = (float)(rand());
       |
       |""".stripMargin
  }

  val compute =
    s"""
       |fun_run(ctx, &fun, outputZ, M, N, inputM, inputX, inputY, alpha, beta);
       |""".stripMargin

  val finish =
    s"""
       |// TODO: could check output here
       |
       |destroyBuffer(ctx, inputM);
       |destroyBuffer(ctx, inputX);
       |destroyBuffer(ctx, inputY);
       |destroyBuffer(ctx, outputZ);
       |""".stripMargin
  // scalastyle:on

  test("print different gemv tuning versions") {

    println("gemvBlastNTuning: " + gemvBlastNTuning)
    println("gemvBlastTTuning: " + gemvBlastTTuning)
    println("gemvFused: " + gemvFusedTuning)
    println("gemvFusedAMDTuning: " + gemvFusedAMDTuning)
    println("gemvKeplerBestTuning: " + gemvKeplerBestTuning)

  }

  def executeGemv(e: Expr, inputSize: Int, s0: Nat) = {

    val params: Nat => Map[Nat, Nat] = s0 => Map(
      TuningParameter("ls0") -> (s0: Nat),
      TuningParameter("ls1") -> (1: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("gs1") -> (1: Nat),
      TuningParameter("s0") -> (s0),
    )

    val eSub = rise.core.substitute.natsInExpr(params(s0), e)

    val result = autotune.execution.execute(
      expression = eSub,
      hostCode = HostCode(init(inputSize, inputSize), compute, finish),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 1000,
      speedupFactor = 1000,
      execution = Minimum
    )
    println("result: " + result.runtime)

  }

  test("execute gemv version") {
    val inputSize = 1 << 12
    println("inputSize: " + inputSize)

    executeGemv(gemvFusedTuning, inputSize, 128) // ignore s0 in this case

    executeGemv(gemvFusedAMDTuning, inputSize, 128) //
    executeGemv(gemvKeplerBestTuning, inputSize, 128) //

    executeGemv(gemvBlastNTuning, inputSize, 64) //
    executeGemv(gemvBlastTTuning, inputSize, 64) //

  }

  test("tune gemv 1024") {
    // change name to run other version
    val version = gemvKeplerBestTuning
    val name = "gemvKeplerBestTuning"

    val configs = Seq(
      "autotuning/config/gemv/1024/rs_cot_1024.json",
      "autotuning/config/gemv/1024/rs_emb_1024.json",
      "autotuning/config/gemv/1024/ls_cot_1024.json",
      "autotuning/config/gemv/1024/atf_emb_1024.json",
      "autotuning/config/gemv/1024/borf_cot_1024.json",
      "autotuning/config/gemv/1024/bogp_cot_1024.json"
    )

    runExperiment(
      name = name,
      configFiles = configs,
      iterations = 2,
      s"autotuning/${name}",
      version,
      HostCode(init(1024, 1024), compute, finish),
      Seq(1024, 1024, 1024)
    )
  }

  test("tune gemv version") {
    runTuning(gemvBlastNTuning)
    runTuning(gemvBlastTTuning)
    runTuning(gemvFusedTuning) // ignore s0 in this case
    runTuning(gemvFusedAMDTuning)
    runTuning(gemvKeplerBestTuning)
  }


  def runTuning(e: Expr) = {
    //    val version = autotuning.parseName(configFile)

    val tuner = Tuner(
      hostCode = HostCode(init(1024, 1024), compute, finish),
      inputSizes = Seq(1024, 1024, 1024),
      samples = 20,
      name = "gemv",
      output = s"autotuning/gemv",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = None,
      hmConstraints = true,
      runtimeStatistic = Minimum,
      saveToFile = true
    )

    autotune.search(tuner)(e)
  }
}