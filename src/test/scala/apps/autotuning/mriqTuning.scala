package apps.autotuning

import apps.autotuning
import apps.mriQ.{computePhiMagOcl, computePhiMagOcl2, computePhiMagOcl3, computeQOcl}
import arithexpr.arithmetic.RangeMul
import rise.autotune
import rise.autotune.{HostCode, Median, Minimum, Timeouts, Tuner, tuningParam, wrapOclRun}
import rise.core.Expr
import rise.core.types.{Nat, TuningParameter}
import shine.OpenCL.{GlobalSize, LocalSize}

class mriqTuning extends test_util.Tests {

//  val computePhiMagTuning:Expr =
//    tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
//      tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
//        tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
//          tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
//              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(computePhiMagOcl)
//            ))))
//

  val computePhiMagTuning:Expr =
    tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
      tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
        tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
          tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
            tuningParam("s0", RangeMul(1, 1024, 2), (s0: Nat) =>
              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(computePhiMagOcl3(s0))
            )))))

  val computeQTuning:Expr =
    tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
      tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
        tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
          tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
            wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(computeQOcl)
          ))))

  // scalastyle:off
  val initPhiMag: (Int) => String = (K) => {
    s"""
       |  const int K = ${K};
       |  srand(time(NULL));
       |  Buffer phiR = createBuffer(ctx, K * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer phiI = createBuffer(ctx, K * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer output = createBuffer(ctx, K * sizeof(float), HOST_READ | DEVICE_WRITE);
       |
       |  float* in_phiR = hostBufferSync(ctx, phiR, K * sizeof(float), HOST_WRITE);
       |  for (int i = 0; i < K; i++) {
       |    in_phiR[i] = (float)(rand() % 100);
       |  }
       |
       |  float* in_phiI = hostBufferSync(ctx, phiI, K * sizeof(float), HOST_WRITE);
       |    for (int i = 0; i < K; i++) {
       |      in_phiI[i] = (float)(rand() % 100);
       |  }
       |
       |
       |  deviceBufferSync(ctx, phiR, K * sizeof(float), DEVICE_READ);
       |  deviceBufferSync(ctx, phiI, K * sizeof(float), DEVICE_READ);
       |""".stripMargin
  }
  val computePhiMag =
    s"""
       |    fun_init_run(ctx, output, K, phiR, phiI);
       |""".stripMargin

  val finishPhiMag =
    s"""
       |  // could add error checking
       |  destroyBuffer(ctx, phiR);
       |  destroyBuffer(ctx, phiI);
       |  destroyBuffer(ctx, output);
       |""".stripMargin

  val initQ: (Int, Int) => String = (K, X) => {
    s"""
       |  const int K = ${K};
       |  const int X = ${X};
       |  srand(time(NULL));
       |  Buffer x = createBuffer(ctx, X * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer y = createBuffer(ctx, X * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer z = createBuffer(ctx, X * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer Qr = createBuffer(ctx, X * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer Qi = createBuffer(ctx, X * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer kvalues = createBuffer(ctx, 4 * K * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer output = createBuffer(ctx, 4 * K * sizeof(float), HOST_READ | DEVICE_WRITE);
       |
       |  float* in_x = hostBufferSync(ctx, x, X * sizeof(float), HOST_WRITE);
       |  float* in_y = hostBufferSync(ctx, y, X * sizeof(float), HOST_WRITE);
       |  float* in_z = hostBufferSync(ctx, z, X * sizeof(float), HOST_WRITE);
       |  for (int i = 0; i < X; i++) {
       |    in_x[i] = (float)(rand() % 100);
       |    in_y[i] = (float)(rand() % 100);
       |    in_z[i] = (float)(rand() % 100);
       |  }
       |
       |  float* in_Qr = hostBufferSync(ctx, Qr, X * sizeof(float), HOST_WRITE);
       |  float* in_Qi = hostBufferSync(ctx, Qi, X * sizeof(float), HOST_WRITE);
       |  for (int i = 0; i < X; i++) {
       |      in_Qr[i] = (float)(rand() % 100);
       |      in_Qi[i] = (float)(rand() % 100);
       |  }
       |
       |  float* in_kvalues = hostBufferSync(ctx, Qi, 4 * K * sizeof(float), HOST_WRITE);
       |  for (int i = 0; i < 4 * K; i++) {
       |    in_kvalues[i] = (float)(rand() % 100);
       |  }
       |
       |  deviceBufferSync(ctx, x, X * sizeof(float), DEVICE_READ);
       |  deviceBufferSync(ctx, y, X * sizeof(float), DEVICE_READ);
       |  deviceBufferSync(ctx, z, X * sizeof(float), DEVICE_READ);
       |
       |  deviceBufferSync(ctx, Qr, X * sizeof(float), DEVICE_READ);
       |  deviceBufferSync(ctx, Qi, X * sizeof(float), DEVICE_READ);
       |
       |  deviceBufferSync(ctx, kvalues, 4 * K * sizeof(float), DEVICE_READ);
       |""".stripMargin
  }
  val computeQ =
    s"""
       |    fun_init_run(ctx, output, K, X, x, y, z, Qr, Qi, kvalues);
       |""".stripMargin

  val finishQ =
    s"""
       |  // could add error checking
       |
       |  destroyBuffer(ctx, x);
       |  destroyBuffer(ctx, y);
       |  destroyBuffer(ctx, z);
       |
       |  destroyBuffer(ctx, Qr);
       |  destroyBuffer(ctx, Qi);
       |
       |  destroyBuffer(ctx, kvalues);
       |
       |  destroyBuffer(ctx, output);
       |""".stripMargin
  // scalastyle:on


  ignore("execute computePhiMag"){

    println("nbody: \n" + computePhiMagTuning)

    val params:Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (256: Nat),
      TuningParameter("ls1") -> (1: Nat),
      TuningParameter("gs0") -> (512: Nat),
      TuningParameter("gs1") -> (1: Nat),
      TuningParameter("s0") -> (8: Nat)
    )

    val phimag_replaced = rise.core.substitute.natsInExpr(params, computePhiMagTuning)
    println("phimag_replaced: \n" + phimag_replaced)

    val result = autotune.execution.execute(
      expression = phimag_replaced,
      hostCode = HostCode(initPhiMag(256), computePhiMag, finishPhiMag),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 10,
      speedupFactor = 100,
        execution = Median
    )

    println("result: " + result)
  }

  ignore("execute computeQ"){

    println("nbody: \n" + computeQTuning)

    val params:Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (64: Nat),
      TuningParameter("ls1") -> (1: Nat),
      TuningParameter("gs0") -> (512: Nat),
      TuningParameter("gs1") -> (1: Nat)
    )

    val q_replaced = rise.core.substitute.natsInExpr(params, computeQTuning)
    println("q_replaced: \n" + q_replaced)

    val result = autotune.execution.execute(
      expression = q_replaced,
      hostCode = HostCode(initQ(256, 512), computeQ, finishQ),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 10,
      speedupFactor = 100,
      execution = Median
    )

    println("result: " + result)
  }

  ignore("tune computePhiMag"){
    val inputSize = 1024

    val tuner = Tuner(
      hostCode = HostCode(initPhiMag(inputSize), computePhiMag, finishPhiMag),
      inputSizes = Seq(inputSize),
      samples = 10,
      name = "computePhiMag",
      output = "autotuning/mriq/computePhiMag",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = None,
      hmConstraints = true,
      runtimeStatistic = Minimum,
      saveToFile = true
    )

    val tuningResult = autotune.search(tuner)(computePhiMagTuning)

    println("tuningResult: \n")
    tuningResult.samples.foreach(elem => println(elem))

    val bestSample = autotune.getBest(tuningResult.samples)
    println("bestSample: \n" + bestSample)
    println("runtime: \n" + bestSample.get.runtime)
  }

  ignore("tune computeQ"){

    val tuner = Tuner(
    hostCode = HostCode(initQ(256, 512), computeQ, finishQ),
    inputSizes = Seq(256, 512),
    samples = 10,
    name = "computeQ",
    output = "autotuning/mriq/computeQ",
    timeouts = Timeouts(10000, 10000, 10000),
    executionIterations = 10,
    speedupFactor = 100,
    configFile = None,
    hmConstraints = true,
    runtimeStatistic = Minimum,
    saveToFile = true
    )

    val tuningResult = autotune.search(tuner)(computeQTuning)

    println("tuningResult: \n")
    tuningResult.samples.foreach(elem => println(elem))

    val bestSample = autotune.getBest(tuningResult.samples)
    println("bestSample: \n" + bestSample)
    println("runtime: \n" + bestSample.get.runtime)
  }

  def runExperiments(configFiles: Seq[String], iterations: Int, tuning: String => autotune.TuningResult) = {

    for(i <- 1 to iterations) {
      configFiles.foreach(tuning)
    }

  }

  def runTuningPhiMag(configFile: String) = {
    val version = rise.autotune.configFileGeneration.parseFromJson(configFile, "application_name")

    val tuner = Tuner(
      hostCode = HostCode(initPhiMag(1024), computePhiMag, finishPhiMag),
      inputSizes = Seq(1024),
      samples = 20, // defined by config file
      name = version,
      output = s"autotuning/mriq/phiMag/${version}",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = Some(configFile),
      hmConstraints = true,
      saveToFile = true
    )

    autotune.search(tuner)(computePhiMagTuning)
  }

  def runTuningQ(configFile: String) = {
    val version = rise.autotune.configFileGeneration.parseFromJson(configFile, "application_name")

    val tuner = Tuner(
      hostCode = HostCode(initQ(1024, 1024), computeQ, finishQ),
      inputSizes = Seq(1024, 1024),
      samples = 20, // defined by config file
      name = version,
      output = s"autotuning/mriq/q/${version}",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = Some(configFile),
      hmConstraints = true,
      saveToFile = true
    )

    autotune.search(tuner)(computeQTuning)
  }

  ignore("run mriq autotuning"){

    // PhiMag
    val configsPhiMag = Seq(
      "autotuning/config/mriq/phiMag/rs_cot_256.json",
    )

    runExperiments(configFiles = configsPhiMag, iterations = 3, runTuningPhiMag)


    // Q
    val configsQ = Seq(
      "autotuning/config/mriq/q/rs_cot_256.json",
    )

    runExperiments(configFiles = configsQ, iterations = 3, runTuningQ)
  }


  ignore("tune computePhiMag 1024"){
    val inputSize: Int = 1024

    val configs = Seq(
      s"autotuning/config/mriq/phiMag/rs_cot_phiMag.json",
      s"autotuning/config/mriq/phiMag/rs_emb_phiMag.json",
      s"autotuning/config/mriq/phiMag/ls_cot_phiMag.json",
      s"autotuning/config/mriq/phiMag/bogp_cot_phiMag.json",
      s"autotuning/config/mriq/phiMag/bogplog_cot_phiMag.json",
      s"autotuning/config/mriq/phiMag/atf_emb_phiMag.json"
    )

    runExperiment(
      name = s"phiMag_${inputSize}",
      configFiles = configs,
      iterations = 5,
      output = s"autotuning/mriq/phiMag",
      computePhiMagTuning,
      HostCode(initPhiMag(inputSize), computePhiMag, finishPhiMag),
      inputSizes = Seq(1024)
    )
  }

  test("tune computeQ 1024"){
    val inputSize: Int = 1024

    val configs = Seq(
      s"autotuning/config/mriq/q/rs_cot_q.json",
      s"autotuning/config/mriq/q/rs_emb_q.json",
      s"autotuning/config/mriq/q/ls_cot_q.json",
      s"autotuning/config/mriq/q/bogp_cot_q.json",
      s"autotuning/config/mriq/q/bogplog_cot_q.json",
      s"autotuning/config/mriq/q/atf_emb_q.json"
    )

    runExperiment(
      name = s"q_${inputSize}",
      configFiles = configs,
      iterations = 10,
      output = s"autotuning/mriq/q",
      computeQTuning,
      HostCode(initQ(inputSize, inputSize), computeQ, finishQ),
      inputSizes = Seq(1024, 1024)
    )
  }
}
