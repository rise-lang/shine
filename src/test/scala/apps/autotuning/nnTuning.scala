package apps.autotuning

import arithexpr.arithmetic.{RangeMul}
import rise.autotune
import rise.autotune.{HostCode, Median, Minimum, Timeouts, Tuner, tuningParam, wrapOclRun}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.types._
import shine.OpenCL.{GlobalSize, LocalSize}

class nnTuning extends test_util.Tests {

  val nn:Expr =
    tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
      tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
        wrapOclRun(LocalSize(ls0), GlobalSize(gs0))(apps.nearestNeighbour.nnOcl)))

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

    val params = Map(
      TuningParameter("ls0") -> (1: Nat),
      TuningParameter("gs0") -> (1: Nat)
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

  test("search nn with generated config file"){
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
      hierarchicalHM = false,
      execution = Minimum
    )

    val tuningResult = autotune.search(tuner)(nn)

    println("tuningResult: \n")
    tuningResult.samples.foreach(elem => println(elem))

    val bestSample = autotune.getBest(tuningResult.samples)
    println("bestSample: \n" + bestSample)
    println("runtime: \n" + bestSample.get.runtime)
  }

}
