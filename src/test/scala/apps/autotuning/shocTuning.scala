package apps.autotuning

import apps.molecularDynamics.{shocOclWithParams}
import arithexpr.arithmetic.{RangeMul, RangeUnknown}
import rise.autotune
import rise.autotune.{HostCode, Median, Minimum, Timeouts, Tuner, tuningParam, wrapOclRun}
import rise.core.types.{Nat, TuningParameter}
import shine.OpenCL.{GlobalSize, LocalSize}
import util.gen

class shocTuning extends test_util.Tests {

  val shocTuning =
    tuningParam("s0", RangeMul(1, 1024, 2), (s0: Nat) =>
      shocOclWithParams(s0))

  val shoc =
    tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
      tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
        tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
          tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
            wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(shocTuning)
          ))))

  // todo update hostcode
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

  // todo fix codegen
  // hostcode-gen does not work with vec(4, f32) types!
  test("generate code shocTuning") {

    val params:Map[Nat, Nat] = Map(
      TuningParameter("s0") -> (128: Nat),
    )

    val shocReplaced = rise.core.substitute.natsInExpr(params, shocTuning)

    val code = gen.opencl.kernel.fromExpr(shocReplaced)

    println("code: \n" + code)

    val codeHosted = gen.opencl.hosted("fun").fromExpr(shocReplaced)
    println("codeHosted: \n" + codeHosted)
  }

  test("execute nbody"){

    val params:Map[Nat, Nat] = Map(
      TuningParameter("s0") -> (128: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("gs1") -> (1: Nat),
      TuningParameter("ls0") -> (128: Nat),
      TuningParameter("ls1") -> (1: Nat),
    )

    val shocReplaced = rise.core.substitute.natsInExpr(params, shoc)

    val result = autotune.execution.execute(
      expression = shocReplaced,
      hostCode = HostCode(init(512), compute, finish),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 10,
      speedupFactor = 100,
      execution = Median,
    )

    println("result: " + result)
  }

  test("tune shoc"){
    val inputSize = 1024

    val configs = Seq(
      s"autotuning/config/shoc/rs_cot_shoc.json",
      s"autotuning/config/shoc/rs_emb_shoc.json",
      s"autotuning/config/shoc/ls_cot_shoc.json",
      s"autotuning/config/shoc/atf_emb_shoc.json",
      s"autotuning/config/shoc/bogp_cot_shoc.json",
      s"autotuning/config/shoc/bogplog_cot_shoc.json"
    )

    runExperiment(
      name = s"shoc",
      configFiles = configs,
      iterations = 10,
      output = s"autotuning/shoc",
      shoc,
      HostCode(init(inputSize), compute, finish),
      Seq(inputSize),
      plotOnly = false
    )
  }
}



