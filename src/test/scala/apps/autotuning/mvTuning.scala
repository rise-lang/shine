package apps.autotuning

import apps.mv.ocl
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

  // mvBlastN
  val mvBlastNTuning: ToBeTyped[Expr] =
    wrapOclMv(mvBlastN)

  // mvBlastN
//  val mvBlastNTuning: ToBeTyped[Expr] =
//    tuningParam("s0", RangeMul(1, 1024, 2), (s0: Nat) =>
//      wrapOclMv(mvBlastNParam(s0))
//    )
//  // mvBlastT
//  val mvBlastTTuning: ToBeTyped[Expr] =
//    tuningParam("s0", RangeMul(1, 1024, 2), (s0: Nat) =>
//      wrapOclMv(mvBlastTParam(s0))
//    )

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
       |int alpha = (float)(rand());
       |int beta = (float)(rand());
       |
       |""".stripMargin
  }

  val compute =
    s"""
       |// fun_run(ctx, &fun, outputZ, M, N, inputM, inputX, inputY, alpha, beta);
       |fun_run(ctx, &fun, outputZ, M, N, inputM, inputX);
       |       |""".stripMargin

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

  test("print different gemv tuning versions"){

    println("mvTuning: " + mvTuning)
    println("mvFusedTuning: " + mvFusedTuning)

    println("mvFusedAMDTuning: " + mvFusedAMDTuning)
    println("mvKeplerBestTuning: " + mvKeplerBestTuning)

    println("mvBlastNTuning: " + mvBlastNTuning)
//    println("mvBlastTTuning: " + mvBlastTTuning)

  }

  def executeMv(e: Expr, s0: Nat) = {

    val params: Nat => Map[Nat, Nat] = s0 => Map(
      TuningParameter("ls0") -> (s0: Nat),
      TuningParameter("ls1") -> (1: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("gs1") -> (1: Nat),
      TuningParameter("s0") -> (s0),
    )

//    val p = rise.autotune.constraints.collectParameters(e)
//    val constraints = rise.autotune.constraints.collectConstraints(e, p)
//    val inputs = rise.autotune.getInputs(e)

//    println("p: " + p)
//    println("inputs: " + inputs) // (m, n) size should be two
//    println("constraint: ")
//    constraints.foreach(println)

    val eSub = rise.core.substitute.natsInExpr(params(s0), e)

    val result = autotune.execution.execute(
      expression = eSub,
      hostCode = HostCode(init(1024, 1024), compute, finish),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 1000,
      speedupFactor = 1000,
      execution = Minimum
    )
    println("result: " + result.runtime)

  }

  test("execute mv version") {

    // update performance numbers
    executeMv(mvTuning, 128) // ignore s0 in this case // 0.05008 ms
    executeMv(mvFusedTuning, 128) // ignore s0 in this case // 0.051008 ms

    executeMv(mvFusedAMDTuning, 128) //
    executeMv(mvKeplerBestTuning, 128) //

    executeMv(mvBlastNTuning, 64) //
    //    executeMv(mvBlastTTuning, 64) //

  }

  test("tune mv version") {
    runTuning(mvTuning, "mvTuning")
    runTuning(mvFusedTuning, "mvFusedTuning")

    runTuning(mvFusedAMDTuning, "mvFusedAMD")
    runTuning(mvKeplerBestTuning, "mvKeplerBest")

    runTuning(mvBlastNTuning, "mvBlastN")
//    runTuning(mvBlastTTuning, "mvBlastT")

  }

  def runTuning(e: Expr, version: String) = {
    //      val version = autotuning.parseName(configFile)

    val tuner = Tuner(
      hostCode = HostCode(init(1024, 1024), compute, finish),
      inputSizes = Seq(1024, 1024),
      samples = 100,
      name = "mv_" + version,
      output = s"autotuning/mv",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = None,
      hmConstraints = true,
      runtimeStatistic = Median,
      saveToFile = true
    )

    autotune.search(tuner)(e)
  }
}