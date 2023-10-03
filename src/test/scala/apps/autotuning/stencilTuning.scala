package apps.autotuning

import arithexpr.arithmetic.RangeMul
import rise.core.DSL._
import rise.core.Expr
import shine.OpenCL.{GlobalSize, LocalSize}
import apps.stencil.acoustic3D.stencilMSS
import rise.autotune._
import rise.core.types.{Nat, TuningParameter}

class stencilTuning extends test_util.Tests {

  private val N = 128
  private val M = 64
  private val O = 32

  val acoustic: Expr =
    tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
      tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
        tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
          tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
            wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(stencilMSS)
          ))))

  // host code
  // scalastyle:off
  val init: (Int, Int, Int) => String = (O, N, M) => {
    s"""
       |const int O = ${O};
       |const int N = ${N};
       |const int M = ${M};
       |
       |srand(time(NULL));
       |
       |Buffer mat1 = createBuffer(ctx, (O+2) * (N+2) * (M+2) * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
       |Buffer mat2 = createBuffer(ctx, (O+2) * (N+2) * (M+2) * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
       |Buffer output = createBuffer(ctx, O * N * M * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_WRITE);
       |
       |float* m1 = hostBufferSync(ctx, mat1, (O+2) * (N+2) * (M+2) * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < (O+2) * (N+2) * (M+2); i++) {
       |  m1[i] = (float)(i%100);
       |}
       |float* m2 = hostBufferSync(ctx, mat2, (O+2) * (N+2) * (M+2) * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < (O+2) * (N+2) * (M+2); i++) {
       |  m2[i] = (float)(i%100);
       |}
       |
       |// synchronize before entering timed section
       |deviceBufferSync(ctx, mat1, (O+2) * (N+2) * (M+2) * sizeof(float), DEVICE_READ);
       |deviceBufferSync(ctx, mat2, (O+2) * (N+2) * (M+2) * sizeof(float), DEVICE_READ);
       |waitFinished(ctx);
       |""".stripMargin
  }

  val compute =
    s"""
       |fun_run(ctx, &fun, output, O, N, M, mat1, mat2);
       |waitFinished(ctx);
       |""".stripMargin

  val finish =
    s"""
       |destroyBuffer(ctx, mat1);
       |destroyBuffer(ctx, mat2);
       |destroyBuffer(ctx, output);
       |""".stripMargin
  // scalastyle:on

  test("run acoustic stencil experiment 1024") {

    val O: Int = 1024
    val N: Int = 1024
    val M: Int = 64

    val expertConfiguration: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (32: Nat),
      TuningParameter("ls1") -> (32: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("gs1") -> (1024: Nat),
    )

    val defaultConfiguration: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (1: Nat),
      TuningParameter("ls1") -> (1: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("gs1") -> (1024: Nat),
    )

    val configs = Seq(
      s"autotuning/config/stencil/1024/rs_cot_1024.json",
      s"autotuning/config/stencil/1024/rs_emb_1024.json",
      s"autotuning/config/stencil/1024/atf_emb_1024.json",
      s"autotuning/config/stencil/1024/bolog_cot_1024.json",
      s"autotuning/config/stencil/1024/ytoptccs_1024.json"
    )

    runExperiment(
      name = s"stencil",
      configFiles = configs,
      iterations = 30,
      output = s"experiment/results/paper/stencil",
      e = acoustic,
      hostCode = HostCode(init(O, N, M), compute, finish),
      inputSizes = Seq(O, N, M), // check whether this is replaced
      plotOnly = false,
      expert = Some(expertConfiguration),
      default = Some(defaultConfiguration)
    )
  }
}
