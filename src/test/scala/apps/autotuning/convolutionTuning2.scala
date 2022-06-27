package apps.autotuning

import apps.separableConvolution2D.{base, baseSeq, baseVecU, binomialWeights2d, binomialWeightsH, binomialWeightsV, factorisedSeq, regRotPar, regRotSeq, scanlinePar, scanlineSeq, separatedSeq}
import apps.separableConvolution2DCheck.wrapExpr
import arithexpr.arithmetic.RangeMul
import rise.autotune.{ExecutionResult, HostCode, Median, RuntimeStatistic, Timeouts, tuningParam, wrapOclRun}
import rise.core.DSL.ToBeTyped
import rise.core.Expr
import rise.core.types.Nat
import rise.openCL.DSL.oclRun
import shine.OpenCL.KernelModule.translationToString
import shine.OpenCL.{GlobalSize, LocalSize}
import util.gen

class convolutionTuning2 extends test_util.Tests {

  private val H = 20
  private val W = 80

  // expressions
  val convolution0: ToBeTyped[Expr] = baseVecU(binomialWeights2d)
  val convolution1: ToBeTyped[Expr] = regRotPar(binomialWeightsV)(binomialWeightsH)
  val convolution2: ToBeTyped[Expr] = scanlinePar(binomialWeightsV)(binomialWeightsH)


  // hostcode
  val init: (Int, Int) => String = (H, W) => {
    s"""
       |const int H = ${H};
       |const int W = ${W};
       |srand(time(NULL));
       |Context ctx = createDefaultContext();
       |fun_t fun;
       |fun_init(ctx, &fun);
       |Buffer matrix = createBuffer(ctx, H * W * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
       |Buffer output = createBuffer(ctx, H * W * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_WRITE);
       |
       |float* m = hostBufferSync(ctx, matrix, H * W * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < H * W; i++) {
       |  m[i] = (float)(rand())/(float)(RAND_MAX) * 10.0f;
       |}
       |
       |// synchronize before entering timed section
       |deviceBufferSync(ctx, matrix, N * N * sizeof(float), DEVICE_READ);
       |waitFinished(ctx);
       |""".stripMargin
  }

  val compute =
    s"""
       |fun_run(ctx, &fun, output, H, W, matrix);
       |waitFinished(ctx);
       |""".stripMargin

  val finish =
    s"""
       |// TODO: could check output here
       |// use given gold expression?
       |
       |destroyBuffer(ctx, matrix);
       |destroyBuffer(ctx, output);
       |fun_destroy(ctx, &fun);
       |destroyContext(ctx);
       |""".stripMargin


  def executeConvolutionDefault(e: Expr) = {

    println("Expression: \n" + e)
    // check this with proper constraints?
    val eOcl = wrapOclRun(LocalSize(16, 8), GlobalSize(1024, 128))(wrapExpr(e))


    println("Expression: \n" + eOcl)

    val result = rise.autotune.execution.execute(
      expression = eOcl,
      hostCode = HostCode(init(H, W), compute, finish),
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      execution = Median
    )

    println("result: \n" + result)

  }

  test("test convolution execution") {

    // we can generate an opencl kernel for these expressions, but Host-code generation fails

    // todo fixme
    // could not solve constraints List((dt741 -> dt741)  ~  (((1+n212).(4+(4*n223)).f32 -> (1+n212).(1+n223).<4>f32) -> _t748))
    executeConvolutionDefault(convolution0)

    // todo fixme
    // inference exception: could not solve constraints List((dt741 -> dt741)  ~  (((1+n445).(4+(4*n378)).f32 -> (1+n445).(4+(4*n378)).f32) -> _t748))
    executeConvolutionDefault(convolution1)

    // todo fixme
    // inference exception: could not solve constraints List((dt741 -> dt741)  ~  (((1+n675).(4*n720).f32 -> (1+n675).(4*n720).f32) -> _t748))
    executeConvolutionDefault(convolution2)

  }

  test("hostcode") {

    println("hostcode: \n" + init(H, W))

    //
    val kernel = gen.opencl.kernel.fromExpr(wrapExpr(convolution2))
    //
    val kernelString = translationToString(kernel)
    println("kernel: \n" + kernelString)

    val test = kernelString.split("\n").map(line => "\"" + line + "\"")

    test.foreach(println)
    //
    //
    //    convolution0

  }

}
