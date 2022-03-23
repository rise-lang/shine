package apps.autotuning

import apps.separableConvolution2D.{base, binomialWeights2d}
import arithexpr.arithmetic.RangeMul
import rise.autotune.{tuningParam, wrapOclRun}
import rise.core.DSL.ToBeTyped
import rise.core.Expr
import rise.core.types.Nat
import rise.openCL.DSL.oclRun
import shine.OpenCL.{GlobalSize, LocalSize}

class convolutionTuning2 extends test_util.Tests {

  // expression
  private val weights2d = binomialWeights2d
  val convolution:Expr = base(weights2d)

  // hostcode
  val init: (Int) => String = (N) => {
    s"""
       |const int N = ${N};
       |srand(time(NULL));
       |Context ctx = createDefaultContext();
       |fun_t fun;
       |fun_init(ctx, &fun);
       |Buffer matrix = createBuffer(ctx, N * N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
       |Buffer weights = createBuffer(ctx, 17 * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
       |Buffer output = createBuffer(ctx, N * N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_WRITE);
       |
       |float* m = hostBufferSync(ctx, matrix, N * N * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < N * N; i++) {
       |  m[i] = (float)(rand())/(float)(RAND_MAX) * 10.0f;
       |}
       |
       |float* w = hostBufferSync(ctx, weights, 17 * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < 17; i++) {
       |  w[i] = (float)(rand())/(float)(RAND_MAX);
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
       |fun_destroy(ctx, &fun);
       |destroyContext(ctx);
       |""".stripMargin

  test("test convolution tuning 2"){

//    val lowering = exploration.strategies.convolutionStrategies.loweringStrategy
//    val lowered:ToBeTyped[Expr] = lowering.apply(base(weights2d)).get
//
//    println("convolution: " +  convolution)
//    println("lowered: " + lowered)
//
//    println("wrap ocl")
//
//
//    val lowered2:Expr = lowered
//    println("lowered2: " + lowered2)
//
//    val test = oclRun(LocalSize(1, 1), GlobalSize(1024, 1024))(lowered)
//
//    println("test: " + test )
//
//    val test2: Expr = test
//    println("test2: " + test)
//
//    val eTuning: Expr = wrapOclRun(LocalSize(1, 1), GlobalSize(1024, 1024))(base(weights2d))
//        println("eTuning: " + eTuning)

    //
    //    val eTuning: Expr =
//      tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
//        tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
//          tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
//            tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
//              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(lowered)
//            ))))

    //    val eTuning: Expr =
//      tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
//        tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
//          tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
//            tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
//              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(lowered.get)
//            ))))


  }




}
