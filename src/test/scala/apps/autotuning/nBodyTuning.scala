package apps.autotuning

import apps.nbody._
import arithexpr.arithmetic.{RangeAdd, RangeMul}
import rise.autotune
import rise.autotune.{HostCode, Median, Minimum, Timeouts, Tuner, tuningParam, wrapOclRun}
import rise.core._
import rise.core.DSL._
import rise.core.primitives.{let => _, _}
import rise.core.DSL.Type._
import rise.core.types._
import shine.OpenCL.{GlobalSize, LocalSize}
import util.gen

class nBodyTuning extends test_util.Tests {

  val nbodyNoTuning = nbodyNVIDIAWithParams(256, 1)

  val nbodyTuning =
    tuningParam("tileX", RangeMul(1, 1024, 2), (tileX: Nat) =>
      tuningParam("tileY", RangeMul(1, 1024, 2), (tileY: Nat) =>
//        tuningParam("vec", RangeMul(1, 1024, 2), (vec: Nat) =>
          nbodyNVIDIAWithParams(tileX, tileY)
        ))

  // scalastyle:off
  val init: (Int) => String = (N) => {
    s"""
       |  const int N = ${N};
       |  srand(time(NULL));
       |  Buffer pos = createBuffer(ctx, 4 * N * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer vel = createBuffer(ctx, 4 * N * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer output = createBuffer(ctx, 4 * N * 2 * sizeof(float), HOST_READ | DEVICE_WRITE);
       |
       |  float* in_pos = hostBufferSync(ctx, pos, 4 * N * sizeof(float), HOST_WRITE);
       |  for (int i = 0; i < 4 * N; i++) {
       |    in_pos[i] = (float)(rand() % 100);
       |  }
       |
       |  float* in_vel = hostBufferSync(ctx, vel, 4 * N * sizeof(float), HOST_WRITE);
       |    for (int i = 0; i < 4 * N; i++) {
       |      in_vel[i] = (float)(rand() % 100);
       |  }
       |
       |  float deltaT = 0.005;
       |  float espSqr = 500.0;
       |
       |  deviceBufferSync(ctx, features, 4 * N * sizeof(float), DEVICE_READ);
       |  deviceBufferSync(ctx, clusters, 4 * N * sizeof(float), DEVICE_READ);
       |""".stripMargin
  }
  val compute =
    s"""
       |    fun_init_run(ctx, output, N, pos, vel, deltaT, espSqr);
       |""".stripMargin

  val finish =
    s"""
       |  // could add error checking
       |  destroyBuffer(ctx, features);
       |  destroyBuffer(ctx, clusters);
       |  destroyBuffer(ctx, output);
       |""".stripMargin
  // scalastyle:on


  // warning: test fails
  // java.lang.Exception: Don't know how to assign value of type <4>f32
  test("execute nbodyNoTuning") {

    println("nbody: \n" + nbodyNoTuning)

    val code = gen.opencl.kernel.fromExpr(nbodyNoTuning)
    println("code: \n" + code)

    val codeHosted = gen.opencl.hosted("fun").fromExpr(nbodyNoTuning)
    println("codeHosted: \n" + codeHosted)
  }

  test("execute nbody"){

    val result = autotune.execution.execute(
      expression = nbodyNoTuning,
      hostCode = HostCode(init(512), compute, finish),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 10,
      speedupFactor = 100,
      execution = Median,
    )

    println("result: " + result)
  }

  test("execute nbody tuning"){

    // could not solve constraints
    val nbody =
      tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
        tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
          tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
            tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(nbodyTuning)
            ))))

    println("nbody: \n" + nbody)

    val params:Map[Nat, Nat] = Map(
      TuningParameter("vec") -> (4: Nat),
      TuningParameter("tileX") -> (256: Nat),
      TuningParameter("tileY") -> (1: Nat),
      TuningParameter("ls0") -> (256: Nat),
      TuningParameter("ls1") -> (1: Nat),
      TuningParameter("gs0") -> (512: Nat),
      TuningParameter("gs1") -> (1: Nat)
    )

    val nbodyReplaced = rise.core.substitute.natsInExpr(params, nbody)

    val result = autotune.execution.execute(
      expression = nbodyReplaced,
      hostCode = HostCode(init(512), compute, finish),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 10,
      speedupFactor = 100,
      execution = Median
    )

    println("result: " + result)
  }

  test("search nbody"){

    println("initalisze")

    // could not solve constraints
    val nbody =
      tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
        tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
          tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
            tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(nbodyTuning)
            ))))

    println("initalized")

    val tuner = Tuner(
      hostCode = HostCode(init(1024), compute, finish),
      inputSizes = Seq(1024),
      samples = 20, // defined by config file
      name = "nbody",
      output = s"autotuning/nbody/nbody",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = None,
      hmConstraints = true,
      runtimeStatistic = Minimum,
      saveToFile = true
    )

    val tuningResult = autotune.search(tuner)(nbody)

    println("tuningResult: \n")
    tuningResult.samples.foreach(elem => println(elem))

    val bestSample = autotune.getBest(tuningResult.samples)
    println("bestSample: \n" + bestSample)
    println("runtime: \n" + bestSample.get.runtime)
  }
}
