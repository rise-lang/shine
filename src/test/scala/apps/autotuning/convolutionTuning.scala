package apps.autotuning

import rise.core.DSL._
import apps.convolution.hosted.{blurYTiled2DTiledLoadingTransposedTuning, convolutionInternal}
import apps.separableConvolution2D._
import arithexpr.arithmetic.RangeMul
import rise.autotune
import rise.autotune.{HostCode, Median, Minimum, Timeouts, Tuner, tuningParam, wrapOclRun}
import rise.core.Expr
import rise.core.types.{Nat, TuningParameter}
import shine.OpenCL.{GlobalSize, LocalSize}

class convolutionTuning extends test_util.Tests {

  val N = 2 << 12

  // expressions
  //  val convolution0: Expr = baseVecU(binomialWeights2d)
  //  val convolution1: Expr = regRotPar(binomialWeightsV)(binomialWeightsH)
  //  val convolution2: Expr = scanlinePar(binomialWeightsV)(binomialWeightsH)


  //  val convolution = blurYTiled2DTiledLoadingTransposedTuning(N)
  val convolution: ToBeTyped[Expr] =
    tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
      tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
        tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
          tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
            tuningParam("s0", RangeMul(1, N, 2), (s0: Nat) =>
              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(convolutionInternal(s0))
            )))))


  // hostcode
  val init: Int => String = N => {
    s"""
       |const int N = ${N};
       |
       |srand(time(NULL));
       |
       |Buffer matrix = createBuffer(ctx, N * N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
       |Buffer weights = createBuffer(ctx, 17 * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
       |Buffer output = createBuffer(ctx, N * N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_WRITE);
       |
       |float* m = hostBufferSync(ctx, matrix, N * N * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < N * N; i++) {
       |  // m[i] = (float)(rand())/(float)(RAND_MAX) * 10.0f;
       |  m[i] =  i + 1;
       |}
       |
       |float* w = hostBufferSync(ctx, weights, 17 * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < 17; i++) {
       |  // w[i] = (float)(rand())/(float)(RAND_MAX);
       |  w[i] =  i + 1;
       |}
       |
       |// synchronize before entering timed section
       |deviceBufferSync(ctx, matrix, N * N * sizeof(float), DEVICE_READ);
       |deviceBufferSync(ctx, weights, 17 * sizeof(float), DEVICE_READ);
       |waitFinished(ctx);
       |""".stripMargin
  }

  val compute =
    s"""
       |fun_run(ctx, &fun, output, N, matrix, weights);
       |waitFinished(ctx);
       |""".stripMargin

  val finish =
    s"""
       |// TODO: could check output here
       |// use given gold expression?
       |
       |destroyBuffer(ctx, matrix);
       |destroyBuffer(ctx, weights);
       |destroyBuffer(ctx, output);
       |""".stripMargin


  def executeConvolutionDefault(e: Expr) = {

    //    println("Expression: \n" + e)

    //    val eOcl = wrapOclRun(LocalSize(1, 1), GlobalSize(1024, 1024))(e)
    val eOcl = e

    //    println("Expression: \n" + eOcl)

    val result = rise.autotune.execution.execute(
      expression = eOcl,
      hostCode = HostCode(init(N), compute, finish),
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 100,
      speedupFactor = 100,
      execution = Median
    )

    println("result: \n" + result)

  }

  ignore("test convolution tuning24") {
    executeConvolutionDefault(convolution)
  }

  test("convolution tuning experiment") {

    val tuner = Tuner(
      hostCode = HostCode(init(N), compute, finish),
      inputSizes = Seq(N),
      samples = 20, // defined by config file, value is ignored
      name = "convolution",
      output = "autotuning",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = None,
      hmConstraints = true,
      runtimeStatistic = Minimum,
      saveToFile = true
    )

    autotune.search(tuner)(convolution)

  }


  test("tune convolution 1024") {
    //    val inputSize: Int = 1024
    val inputSize = N
    println("N: " + N)

    // expert configuration
    val expertConfiguration: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (8: Nat),
      TuningParameter("ls1") -> (16: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("gs1") -> (128: Nat),
      TuningParameter("s0") -> (8: Nat),
    )

    val defaultConfiguration: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (1: Nat),
      TuningParameter("ls1") -> (1: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("gs1") -> (1024: Nat),
      TuningParameter("s0") -> (1: Nat),
    )

    val configs = Seq(
      //      s"autotuning/config/convolution/${inputSize.toString}/rs_cot_${inputSize.toString}.json",
      //      s"autotuning/config/convolution/${inputSize.toString}/rs_emb_${inputSize.toString}.json",
      //      s"autotuning/config/convolution/${inputSize.toString}/bogp_cot_${inputSize.toString}.json",
      //      s"autotuning/config/convolution/${inputSize.toString}/bogplsp_cot_${inputSize.toString}.json",
      s"autotuning/config/convolution/${inputSize.toString}/atf_emb_${inputSize.toString}.json"
    )

    runExperiment(
      name = s"convolution_${inputSize}",
      configFiles = configs,
      iterations = 3,
      s"experiment/results/convolution_${inputSize}",
      convolution,
      HostCode(init(inputSize), compute, finish),
      Seq(inputSize),
      plotOnly = false,
      expert = Some(expertConfiguration),
      default = Some(defaultConfiguration)
    )
  }

}
