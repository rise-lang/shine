package apps.autotuning

import arithexpr.arithmetic.{RangeAdd, RangeMul}
import rise.autotune
import rise.autotune.{HostCode, Median, Timeouts, Tuner, tuningParam, wrapOclRun}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.primitives.{join, split}
import rise.core.types._
import rise.openCL.DSL._
import shine.OpenCL.{GlobalSize, LocalSize}

class nnTuning extends test_util.Tests {

  private val distance = foreignFun("distance_",
    Seq("loc", "lat", "lng"),
    "{ return sqrt((lat - loc._fst) * (lat - loc._fst) + (lng - loc._snd) * (lng -  loc._snd)); }",
    (f32 x f32) ->: f32 ->: f32 ->: f32
  )

  val nnTuningGlb2: Expr =
  tuningParam("s0", RangeAdd(1, 1024, 1),(s0: Nat) =>
    depFun((n: Nat) => fun(
      (n `.` (f32 x f32)) ->: f32 ->: f32 ->: (n `.` f32)
    )((locations, lat, lng) =>
      locations |> split(s0) |> mapGlobal(0)(mapGlobal(1)(fun(loc => distance(loc)(lat)(lng)))) |> join
    )))

  val nnTuningWrgLcl: Expr =
    tuningParam("s0", RangeAdd(1, 1024, 1),(s0: Nat) =>
      depFun((n: Nat) => fun(
        (n `.` (f32 x f32)) ->: f32 ->: f32 ->: (n `.` f32)
      )((locations, lat, lng) =>
        locations |> split(s0) |> mapWorkGroup(mapLocal(fun(loc => distance(loc)(lat)(lng)))) |> join
      )))

  //  val nnTuningWrgLcl2: Expr =
  //    tuningParam("s0", RangeAdd(1, 1024, 1),(s0: Nat) =>
  //      tuningParam("s1", RangeAdd(1, 1024, 1),(s1: Nat) =>
  //        depFun((n: Nat) => fun(
  //          (n `.` (f32 x f32)) ->: f32 ->: f32 ->: (n `.` f32)
  //        )((locations, lat, lng) =>
  //          locations |> split(s0) |>
  //            mapWorkGroup(0)(mapLocal(0)(
  //              fun(loc2 => loc2 |> split(s1) |>
  //                mapWorkGroup(1)(mapLocal(1)(
  //                  fun(loc => distance(loc)(lat)(lng))
  //                )) |> join
  //              )
  //            )) |> join
  //        ))))

  val nn:Expr =
    tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
      tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
        wrapOclRun(LocalSize(ls0), GlobalSize(gs0))(apps.nearestNeighbour.nnOcl)))

  val nnGlb2:Expr =
    tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
      tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
        tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
          tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
            wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(nnTuningGlb2)))))

  val nnWrgLcl:Expr =
    tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
      tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
        tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
          tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
            wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(nnTuningWrgLcl)))))

  //  val nnWrgLcl2:Expr =
  //    tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
  //      tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
  //        tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
  //          tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
  //            wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(nnTuningWrgLcl2)))))

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


  test("execute nn"){

    println("nn: \n" + nn)

    val params = Map(
      TuningParameter("ls0") -> (1: Nat),
      TuningParameter("gs0") -> (1: Nat)
    )

    val nn_replaced = rise.core.substitute.natsInExpr(params, nn)
    println("nn_replaced: \n" + nn_replaced)

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

  test("search nn manual config file"){

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
      //      configFile = Some("/home/jo/development/rise-lang/shine/autotuning/config/nn_1024.json"),
//      hierarchicalHM = true,
      execution = Median
    )

    // tune
    val nnResult  = autotune.search(tuner)(nn)
    val nnWrgLclResult = autotune.search(tuner)(nnGlb2)
    val nnGlb2Result = autotune.search(tuner)(nnWrgLcl)
    //    val nnWrgLcl2Result = autotune.search(tuner)(nnWrgLcl2)

    // print results
    println("nn: \n" + autotune.getBest(nnResult.samples))
    println("nnGlb2: \n" + autotune.getBest(nnGlb2Result.samples))
    println("nnWrgLcl: \n" + autotune.getBest(nnWrgLclResult.samples))
    //    println("nnWrgLcl2: \n" + autotune.getBest(nnWrgLcl2Result.samples))

  }
}
