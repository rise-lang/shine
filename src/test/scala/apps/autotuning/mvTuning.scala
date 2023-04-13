package apps.autotuning

import apps.mv.ocl._
import arithexpr.arithmetic.RangeMul
import rise.autotune
import rise.autotune._
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.types._
import shine.OpenCL.{GlobalSize, LocalSize}

class mvTuning extends test_util.Tests {

  // mv
  val mvTuning: ToBeTyped[Expr] =
    wrapOclMv(mv)

  // mvFused
  val mvFusedTuning: ToBeTyped[Expr] =
    wrapOclMv(mvFused)

  val mvFusedSplitTuning: ToBeTyped[Expr] =
    wrapOclMv(mvFusedSplit)

  // mvBlastN
  val mvBlastNTuning: ToBeTyped[Expr] =
    tuningParam("s0", RangeMul(1, 1024, 2), (s0: Nat) =>
      wrapOclMv(mvBlastNParam(s0))
    )

  // mvBlastT
  val mvBlastTTuning: ToBeTyped[Expr] =
    tuningParam("s0", RangeMul(1, 1024, 2), (s0: Nat) =>
      wrapOclMv(mvBlastTParam(s0))
    )

  // gemvFusedAMD
  val mvFusedAMDTuning: ToBeTyped[Expr] =
    tuningParam("s0", RangeMul(1, 1024, 2), (s0: Nat) =>
      wrapOclMv(mvFusedAMDParam(s0))
    )

  // mvKeplerBest
  val mvKeplerBestTuning: ToBeTyped[Expr] =
    tuningParam("s0", RangeMul(1, 1024, 2), (s0: Nat) =>
      wrapOclMv(mvKeplerBestParam(s0))
    )

  def wrapOclMv(e: Expr): Expr = {
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
       |Buffer outputZ = createBuffer(ctx, M * sizeof(float), HOST_READ | DEVICE_WRITE);
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
       |""".stripMargin
  }

  val compute =
    s"""
       |fun_run(ctx, &fun, outputZ, M, N, inputM, inputX);
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

  def executeMv(e: Expr, inputSize: Int, s0: Nat) = {

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

  test("execute mv version") {
    val inputSize = 1 << 12
    println("inputSize: " + inputSize)

    // update performance numbers
    executeMv(mvTuning, inputSize, 128) // ignore s0 in this case // 0.05008 ms
    executeMv(mvFusedTuning, inputSize, 128) // ignore s0 in this case // 0.051008 ms
    executeMv(mvFusedSplitTuning, inputSize, 128) // ignore s0 in this case // 0.051008 ms

    executeMv(mvFusedAMDTuning, inputSize, 128) //
    executeMv(mvKeplerBestTuning, inputSize, 128) //

    executeMv(mvBlastNTuning, inputSize, 64) //
    executeMv(mvBlastTTuning, inputSize, 64) //

  }

  test("tune gemv 1024") {
    // change name to run other version
    val version = mvKeplerBestTuning
    val name = "mvKeplerBestTuning"

    // expert configuration
    val expertConfiguration: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (32: Nat),
      TuningParameter("ls1") -> (32: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("gs1") -> (1024: Nat),
      TuningParameter("s0") -> (32: Nat),
    )

    // default configuration
    //    val defaultConfiguration: Map[Nat, Nat] = Map(
    //      TuningParameter("ls0") -> (32: Nat),
    //      TuningParameter("ls1") -> (8: Nat),
    //      TuningParameter("gs0") -> (256: Nat),
    //      TuningParameter("gs1") -> (128: Nat),
    //      TuningParameter("s0") -> (4: Nat),
    //    )


    val configs = Seq(
      "autotuning/config/mv/1024/rs_cot_1024.json",
      //      "autotuning/config/mv/1024/rs_emb_1024.json",
      //      "autotuning/config/mv/1024/ls_cot_1024.json",
      //      "autotuning/config/mv/1024/opentuner.json",
      //      "autotuning/config/mv/1024/borf_cot_1024.json",
      //      "autotuning/config/mv/1024/bogp_cot_1024.json"
    )

    runExperiment(
      name = name,
      configFiles = configs,
      iterations = 2,
      s"autotuning/${name}",
      version,
      HostCode(init(1024, 1024), compute, finish),
      Seq(1024, 1024, 1024),
      expert = Some(expertConfiguration)
    )
  }
}