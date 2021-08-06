package apps.autotuning

import rise.core.DSL._
import rise.core.DSL.Type._
import HighLevelConstructs._
import apps.separableConvolution2D
import arithexpr.arithmetic.RangeMul
import rise.autotune
import rise.autotune.{HostCode, Median, Timeouts, tuningParam, wrapOclRun}
import rise.core._
import rise.core.primitives._
import rise.core.types._
import rise.openCL.DSL._
import rise.openCL.primitives.oclReduceSeqUnroll
import shine.OpenCL.{GlobalSize, LocalSize}

class convolutionTuning extends test_util.Tests {

  private val id = fun(x => x)

  private val dotElemWeights = fun((weights, elem) =>
    zip(join(elem))(weights) |>
      map(separableConvolution2D.mulT) |>
      reduce(add)(lf32(0.0f))
  )

  private val dotElemWeightsSeq = fun((weights, elem) =>
    oclReduceSeqUnroll(AddressSpace.Private)(fun((acc, pair) => {
      val pixel = pair._1
      val weight = pair._2
      acc + (pixel * weight)
    }))(lf32(0.0f))(zip(join(elem))(weights)))

  val blurXTiled2D: ToBeTyped[Expr] = depFun((n: Nat) => fun(
    (n `.` n `.` f32) ->: (17 `.` f32) ->: (n `.` n `.` f32)
  )((matrix, weights) =>
    unslide2D o mapWorkGroup(1)(mapWorkGroup(0)(fun(tile =>
      mapLocal(1)(mapLocal(0)(dotElemWeightsSeq(weights)))
        o slide2D(1, 1, 17, 1)
        $ toLocal(mapLocal(1)(mapLocal(0)(id))(tile)))
    )) o slide2D(4, 4, 144, 128)
      o padClamp2D(0, 0, 8, 8) $ matrix
  ))


  //   could not solve constraints
  //  val blurYyo:Expr = blurYTiled2DTiledLoadingTransposed
  //    tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
  //      tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
  //        tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
  //          tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
  //            wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(blurYTiled2DTiledLoadingTransposed)
  //          ))))
  //
  //val blurYTiled2DTiledLoadingTransposed: ToBeTyped[Expr] =
  //depFun((n: Nat) =>
  //  fun(
  //    (n `.` n `.` f32) ->: (17 `.` f32) ->: (n `.` n `.` f32)
  //  )((matrix, weights) =>
  //    unslide2D o mapWorkGroup(1)(mapWorkGroup(0)(fun(tile =>
  //      mapLocal(1)(mapLocal(0)(dotElemWeightsSeq(weights)))
  //        o slide2D(17, 1, 1, 1)
  //        o transpose o map(dropLast(1)) $ toLocal(
  //        transpose(tile)
  //          |> map(split(8))
  //          |> mapLocal(0)(mapSeqUnroll(mapLocal(1)(id)))
  //          |> map(join >> padEmpty(1))
  //      )
  //    ))) o slide2D(80, 64, 16, 16)
  //      o padClamp2D(8, 8, 0, 0) $ matrix
  //  ))

  val init: (Int) => String = (N) => {
    s"""
       |const int N = ${N};
       |srand(time(NULL));
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
       |deviceBufferSync(ctx, output, N * N * sizeof(float), DEVICE_WRITE);
       |       |""".stripMargin
  }

  val compute =
    s"""
       |fun_run(ctx, &fun, output, N, matrix, weights);
       |""".stripMargin

  val finish =
    s"""
       |// TODO: could check output here
       |
       |destroyBuffer(ctx, matrix);
       |destroyBuffer(ctx, weights);
       |destroyBuffer(ctx, output);
       |""".stripMargin

  //  val stats = Seq(
  //    ("dpia X", hostedBenchmark(sampleCount, "one_copy", init, compute, finish, hostedX)),
  //    ("dpia Y", hostedBenchmark(sampleCount, "one_copy", init, compute, finish, hostedY)),


  test("execute X"){
    // hostedX
    val N = 4096
    val x:Expr = blurXTiled2D
    println("x: \n" + x)

    val x_ocl: Expr =
      tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
        tuningParam("ls1",  RangeMul(1, 1024, 2),(ls1: Nat) =>
          tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
            tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(x)
            ))))


    println("x ocl: \n" + x_ocl)

    val params = Map(
      TuningParameter("ls0") -> (16: Nat),
      TuningParameter("ls1") -> (4: Nat),
      TuningParameter("gs0") -> (N/8: Nat),
      TuningParameter("gs1") -> (N: Nat)
    )

    val x_replaced = rise.core.substitute.natsInExpr(params, x_ocl)
    println("x_replaced: \n" + x_replaced)

    val result = autotune.execution.execute(
      expression = x_replaced,
      hostCode = HostCode(init(N), compute, finish),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 10,
      speedupFactor = 100,
      execution = Median
    )

    println("result: " + result)

  }

//  test("execute Y"){
//    // hostedX
//    val N = 4096
//    val y = blurYTiled2DTiledLoadingTransposedOCL(N)
//    //    val y = blurYyo
//
//    println("y: \n" + y)
//
//    val params = Map(
//      TuningParameter("ls0") -> (16: Nat),
//      TuningParameter("ls1") -> (4: Nat),
//      TuningParameter("gs0") -> (N/8: Nat),
//      TuningParameter("gs1") -> (N: Nat)
//    )
//
//    val y_replaced = rise.core.substitute.natsInExpr(params, y)
//    println("y_replaced: \n" + y_replaced)
//
//    println("generate code")
//    val codeHosted = gen.opencl.hosted("fun").fromExpr(y_replaced)
//    println("codeHosted: " + codeHosted)
//
//    val result = autotune.execution.execute(
//      expression = y,
//      hostCode = HostCode(init(N), compute, finish),
//      timeouts = Timeouts(5000, 5000, 5000),
//      executionIterations = 10,
//      speedupFactor = 100,
//      execution = Median
//    )
//    println("result: " + result)
//  }

}
