package apps.autotuning

import apps.autotuning
import arithexpr.arithmetic.RangeMul
import rise.autotune
import rise.autotune.{HostCode, Median, Minimum, Timeouts, Tuner, tuningParam, wrapOclRun}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.types._
import shine.OpenCL.{GlobalSize, LocalSize}

class nnTuning extends test_util.Tests {

  val nn: Expr =
    tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
      tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
        tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
          tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
            wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(apps.nearestNeighbour.nnOcl)
          ))))

  // scalastyle:off
  val init: (Int) => String = (N) => {
    s"""
       |  const int N = ${N};
       |  srand(time(NULL));
       |  Buffer input = createBuffer(ctx, 2 * N * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer output = createBuffer(ctx, N * sizeof(float), HOST_READ | DEVICE_WRITE);
       |
       |  float* in = hostBufferSync(ctx, input, 2 * N * sizeof(float), HOST_WRITE);
       |  for (int i = 0; i < 2 * N ; i++) {
       |    in[i] = (float)(rand() % 100);
       |  }
       |
       |  float lat = (float)(rand() % 100);
       |  float lng = (float)(rand() % 100);
       |
       |  deviceBufferSync(ctx, input, 2 * N * sizeof(float), DEVICE_READ);
       |""".stripMargin
  }

  val compute =
    s"""
       |    fun_init_run(ctx, output, N, input, lat, lng);
       |       |""".stripMargin

  val finish =
    s"""
       |  // could add error checking
       |  destroyBuffer(ctx, input);
       |  destroyBuffer(ctx, output);
       |""".stripMargin
  // scalastyle:on


  ignore("execute nn") {

    val params: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (1: Nat),
      TuningParameter("ls1") -> (1: Nat),
      TuningParameter("gs0") -> (1: Nat),
      TuningParameter("gs1") -> (1: Nat)
    )

    val nn_replaced = rise.core.substitute.natsInExpr(params, nn)

    val result = autotune.execution.execute(
      expression = nn_replaced,
      hostCode = HostCode(init(1024), compute, finish),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 10,
      speedupFactor = 100,
      execution = Median
    )

    println("result: " + result)
  }

  ignore("search nn with generated config file") {
    val tuner = Tuner(
      hostCode = HostCode(init(1024), compute, finish),
      inputSizes = Seq(1024),
      samples = 10,
      name = "nn",
      output = "autotuning/nn",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = None,
      hmConstraints = true,
      runtimeStatistic = Minimum,
      saveToFile = true
    )

    val tuningResult = autotune.search(tuner)(nn)

    println("tuningResult: \n")
    tuningResult.samples.foreach(elem => println(elem))

    val bestSample = autotune.getBest(tuningResult.samples)
    println("bestSample: \n" + bestSample)
    println("runtime: \n" + bestSample.get.runtime)
  }


  def runExperiments(configFiles: Seq[String], iterations: Int) = {
    for (i <- 1 to iterations) {
      configFiles.foreach(runTuning)
    }
  }

  def runTuning(configFile: String) = {
    val version = rise.autotune.configFileGeneration.parseFromJson(configFile, "application_name")

    val tuner = Tuner(
      hostCode = HostCode(init(1024), compute, finish),
      inputSizes = Seq(1024),
      samples = 20, // defined by config file
      name = version,
      output = s"autotuning/nn/${version}",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = Some(configFile),
      hmConstraints = true,
      saveToFile = true
    )
    autotune.search(tuner)(nn)
  }

  test("run nn autotuning") {

    val inputSize: Int = 1024

    // expert configuration
    val expertConfiguration: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (128: Nat),
      TuningParameter("ls1") -> (1: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("gs1") -> (1: Nat)
    )

    val defaultConfiguration: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (1: Nat),
      TuningParameter("ls1") -> (1: Nat),
      TuningParameter("gs0") -> (1: Nat),
      TuningParameter("gs1") -> (1: Nat)
    )

    val configs = Seq(
      s"autotuning/config/nn/${inputSize.toString}/rs_cot_${inputSize.toString}.json",
      s"autotuning/config/nn/${inputSize.toString}/rs_emb_${inputSize.toString}.json",
      s"autotuning/config/nn/${inputSize.toString}/bogp_cot_${inputSize.toString}.json",
      s"autotuning/config/nn/${inputSize.toString}/bogplsp_cot_${inputSize.toString}.json",
      s"autotuning/config/nn/${inputSize.toString}/atf_emb_${inputSize.toString}.json"
    )

    runExperiment(
      name = s"nn_${inputSize}",
      configFiles = configs,
      iterations = 10,
      output = s"/home/jo/development/experiments/tuning/results/nn_${inputSize}",
      //      s"experiment/results/nn_${inputSize}",
      e = nn,
      hostCode = HostCode(init(inputSize), compute, finish),
      inputSizes = Seq(inputSize),
      plotOnly = true,
      expert = Some(expertConfiguration),
      default = Some(defaultConfiguration)
    )
  }

}
