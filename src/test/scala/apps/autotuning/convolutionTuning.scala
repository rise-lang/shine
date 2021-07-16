package apps.autotuning

import rise.core.DSL._
import rise.core.DSL.Type._
import HighLevelConstructs._
import apps.separableConvolution2D
import rise.autotune
import rise.autotune.{HostCode, Timeouts, tuningParam, wrapOclRun}
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


//  val x: Expr = {
//    tuningParam("n", (n: Nat) =>
//    tuningParam("ls0", (ls0: Nat) => tuningParam("ls1", (ls1: Nat) =>
//      tuningParam("gs0", (gs0: Nat) => tuningParam("gs1", (gs1: Nat) =>
//        wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(blurXTiled2D(n)))))))
//  }

  val blurXTiled2DOCL: ToBeTyped[Expr] =
//    depFun((n: Nat) =>
    tuningParam("n", (n: Nat) =>
      tuningParam("ls0", (ls0: Nat) =>
      tuningParam("ls1", (ls1: Nat) =>
        tuningParam("gs0", (gs0: Nat) =>
          tuningParam("gs1", (gs1: Nat) =>
              oclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(blurXTiled2D(n)
            ))))))

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


  val blurYTiled2DTiledLoadingTransposedOCL: ToBeTyped[Expr] = depFun((n: Nat) =>
    tuningParam("ls0", (ls0: Nat) =>
      tuningParam("ls1", (ls1: Nat) =>
        tuningParam("gs0", (gs0: Nat) =>
          tuningParam("gs1", (gs1: Nat) =>
            fun(
              (n`.`n`.`f32) ->: (17`.`f32) ->: (n`.`n`.`f32)
            )((matrix, weights) =>
              oclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(
                blurYTiled2DTiledLoadingTransposed(n)(matrix)(weights))
            ))))))

  val blurYTiled2DTiledLoadingTransposed: ToBeTyped[Expr] =
    depFun((n: Nat) =>
      tuningParam("sp0", (sp0: Nat) =>
        fun(
      (n `.` n `.` f32) ->: (17 `.` f32) ->: (n `.` n `.` f32)
    )((matrix, weights) =>
      unslide2D o mapWorkGroup(1)(mapWorkGroup(0)(fun(tile =>
        mapLocal(1)(mapLocal(0)(dotElemWeightsSeq(weights)))
          o slide2D(17, 1, 1, 1)
          o transpose o map(dropLast(1)) $ toLocal(
          transpose(tile)
            |> map(split(sp0))
            |> mapLocal(0)(mapSeqUnroll(mapLocal(1)(id)))
            |> map(join >> padEmpty(1))
        )
      ))) o slide2D(80, 64, 16, 16)
        o padClamp2D(8, 8, 0, 0) $ matrix
    )))

  val init: (Int) => String = (N) => {
    s"""
       |srand(time(NULL));
       |Context ctx = createDefaultContext();
       |fun_t fun;
       |fun_init(ctx, &fun);
       |Buffer matrix = createBuffer(ctx, $N * $N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
       |Buffer weights = createBuffer(ctx, 17 * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
       |Buffer output = createBuffer(ctx, $N * $N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_WRITE);
       |
       |float* m = hostBufferSync(ctx, matrix, $N * $N * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < $N * $N; i++) {
       |  m[i] = (float)(rand())/(float)(RAND_MAX) * 10.0f;
       |}
       |
       |float* w = hostBufferSync(ctx, weights, 17 * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < 17; i++) {
       |  w[i] = (float)(rand())/(float)(RAND_MAX);
       |}
       |
       |// synchronize before entering timed section
       |deviceBufferSync(ctx, matrix, $N * $N * sizeof(float), DEVICE_READ);
       |deviceBufferSync(ctx, weights, 17 * sizeof(float), DEVICE_READ);
       |waitFinished(ctx);
       |""".stripMargin
  }

  val compute =
    s"""
       |fun_run(ctx, &fun, output, matrix, weights);
       |waitFinished(ctx);
       |""".stripMargin

  val finish =
    s"""
       |// TODO: could check output here
       |
       |destroyBuffer(ctx, matrix);
       |destroyBuffer(ctx, weights);
       |destroyBuffer(ctx, output);
       |fun_destroy(ctx, &fun);
       |destroyContext(ctx);
       |""".stripMargin

//  val stats = Seq(
//    ("dpia X", hostedBenchmark(sampleCount, "one_copy", init, compute, finish, hostedX)),
//    ("dpia Y", hostedBenchmark(sampleCount, "one_copy", init, compute, finish, hostedY)),


    test("execute X"){
      // hostedX
      val N = 4096
      val x:Expr = blurXTiled2DOCL(N)

      println("x: \n" + x)

      val params = Map(
        TuningParameter("ls0") -> (16: Nat),
        TuningParameter("ls1") -> (4: Nat),
        TuningParameter("gs0") -> (N/8: Nat),
        TuningParameter("gs1") -> (N: Nat)
      )

      val x_replaced = rise.core.substitute.natsInExpr(params, x)
      println("x_replaced: \n" + x_replaced)

      val result = autotune.execution.execute(
        expression = x,
        hostCode = HostCode(init(N), compute, finish),
        timeouts = Timeouts(5000, 5000, 5000),
        executionIterations = 10,
        speedupFactor = 100
      )

      println("result: " + result)

    }

  test("execute Y"){
    // hostedX
    val N = 4096
    val y = blurYTiled2DTiledLoadingTransposedOCL(N)

    println("y: \n" + y)

    val params = Map(
      TuningParameter("ls0") -> (16: Nat),
      TuningParameter("ls1") -> (4: Nat),
      TuningParameter("gs0") -> (N/8: Nat),
      TuningParameter("gs1") -> (N: Nat)
    )

    val y_replaced = rise.core.substitute.natsInExpr(params, y)
    println("y_replaced: \n" + y_replaced)

    val result = autotune.execution.execute(
      expression = y,
      hostCode = HostCode(init(N), compute, finish),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 10,
      speedupFactor = 100
    )

    println("result: " + result)

  }


}
