package apps.autotuning

import rise.autotune
import rise.autotune.{HostCode, Timeouts, Tuner, tuningParam, wrapOclRun}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.types._
import rise.openCL.DSL._
import shine.OpenCL.{GlobalSize, LocalSize}

class nnTuning extends test_util.Tests {

  // define nn
  private val distance = foreignFun("distance_",
    Seq("loc", "lat", "lng"),
    "{ return sqrt((lat - loc._fst) * (lat - loc._fst) + (lng - loc._snd) * (lng -  loc._snd)); }",
    (f32 x f32) ->: f32 ->: f32 ->: f32
  )

  val nnTuning: Expr =
    tuningParam("n", (n: Nat)
    => fun(
        (n `.` (f32 x f32)) ->: f32 ->: f32 ->: (n `.` f32)
      )((locations, lat, lng) =>
        locations |> mapGlobal(fun(loc => distance(loc)(lat)(lng)))
      ))

  val nn:Expr =
  tuningParam("ls0", (ls0: Nat) =>
    tuningParam("gs0", (gs0: Nat) =>
      wrapOclRun(LocalSize(ls0), GlobalSize(gs0))(nnTuning)))

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
       |    // fun_init_run(ctx, output, N, input, lat, lng);
       |    fun_init_run(ctx, output, input, lat, lng);
       |       |""".stripMargin

  val finish =
    s"""
       |  // could add error checking
       |  destroyBuffer(ctx, input);
       |  destroyBuffer(ctx, output);
       |""".stripMargin


  test("execute nn"){

    println("nn: \n" + nn)

    val params = Map(
      TuningParameter("n") -> (1024: Nat),
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
      speedupFactor = 100
    )

    println("result: " + result)
  }

  test("get constraints"){

    val params = autotune.constraints.collectParameters(nn)
    val constraints = autotune.constraints.collectConstraints(nn, params)

    params.foreach(param => {
      println("param: " + param)
      println("param: " + param.range)
    })

    constraints.foreach(constraint => {
      println("constraint: " + constraint)
    })

  }

  test("search nn"){

    val tuner = Tuner(
      hostCode = HostCode(init(1024), compute, finish),
      samples = 100,
      name = "nn",
      output = "autotuning/nn",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile =Some("/home/jo/development/rise-lang/shine/autotuning/config/nn_1024.json"),
      true
    )

    val tuningResult = autotune.search(tuner)(nn)

    println("tuningResult: \n")
    tuningResult.samples.foreach(elem => println(elem))

    val bestSample = autotune.getBest(tuningResult.samples)
    println("bestSample: \n" + bestSample)
    println("runtime: \n" + bestSample.get.runtime)
  }
}
