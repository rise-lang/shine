package apps.autotuning

import apps.nbody.{nbodyNVIDIDAWithParams}
import rise.autotune
import rise.autotune.{HostCode, Timeouts, Tuner, tuningParam, wrapOclRun}
import rise.core._
import rise.core.DSL._
import rise.core.primitives.{let => _, _}
import rise.core.DSL.Type._
import rise.core.types._
import shine.OpenCL.{GlobalSize, LocalSize}

class nBodyTuning extends test_util.Tests {

  val nbodyNoTuning = nbodyNVIDIDAWithParams(256, 1)

  // could not solve constraints
  val nbodyTuning:ToBeTyped[Expr] =
    tuningParam("n", (n: Nat) =>
      tuningParam("tileX", (tileX: Nat) =>
        tuningParam("tileY", (tileY: Nat) =>
          nbodyNVIDIDAWithParams(n, tileX, tileY)
        )))

  val nbody:Expr =
    tuningParam("ls0", (ls0: Nat) =>
      tuningParam("ls1", (ls1: Nat) =>
        tuningParam("gs0", (gs0: Nat) =>
          tuningParam("gs1", (gs1: Nat) =>
            wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(nbodyTuning)
          ))))

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


  test("execute nbodyNoTuning"){
    // java.lang.Exception: Don't know how to assign value of type <4>f32

    println("nbody: \n" + nbodyNoTuning)

    val result = autotune.execution.execute(
      expression = nbodyNoTuning,
      hostCode = HostCode(init(512), compute, finish),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 10,
      speedupFactor = 100
    )

    println("result: " + result)
  }

  test("execute nbody"){

    println("nbody: \n" + nbody)

    val params = Map(
      TuningParameter("n") -> (512: Nat),
      TuningParameter("tileX") -> (256: Nat),
      TuningParameter("tileY") -> (1: Nat),
      TuningParameter("ls0") -> (256: Nat),
      TuningParameter("ls1") -> (1: Nat),
      TuningParameter("gs0") -> (512: Nat),
      TuningParameter("gs1") -> (1: Nat)
    )

    val kmeans_replaced = rise.core.substitute.natsInExpr(params, nbody)
    println("kmeans_replaced: \n" + kmeans_replaced)

    val result = autotune.execution.execute(
      expression = kmeans_replaced,
      hostCode = HostCode(init(512), compute, finish),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 10,
      speedupFactor = 100
    )

    println("result: " + result)
  }

  test("nbody get constraints"){
    val params = autotune.constraints.collectParameters(nbody)
    val constraints = autotune.constraints.collectConstraints(nbody, params)

    params.foreach(param => {
      println("param: " + param)
      println("param: " + param.range)
    })

    constraints.foreach(constraint => {
      println("constraint: " + constraint)
    })
  }

  test("search nbody"){

    val tuner = Tuner(
      hostCode = HostCode(init(1024), compute, finish),
      samples = 100,
      name = "nbody",
      output = "autotuning/nbody",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = Some("/home/jo/development/rise-lang/shine/autotuning/config/nbody_1024.json"),
      true
    )

    val tuningResult = autotune.search(tuner)(nbody)

    println("tuningResult: \n")
    tuningResult.samples.foreach(elem => println(elem))

    val bestSample = autotune.getBest(tuningResult.samples)
    println("bestSample: \n" + bestSample)
    println("runtime: \n" + bestSample.get.runtime)
  }
}
