package apps.autotuning

import arithexpr.arithmetic.RangeMul
import rise.autotune
import rise.autotune.{HostCode, Median, Minimum, Timeouts, Tuner, tuningParam, wrapOclRun}
import rise.core._
import rise.core.types.{Nat, _}
import shine.OpenCL.{GlobalSize, LocalSize}

class kmeansTuning extends test_util.Tests {

  val kmeans: Expr =
    tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
      tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
        tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
          tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
            wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(apps.kmeans.kmeansOcl)
          ))))


  // scalastyle:off
  val init: (Int, Int, Int) => String = (p, c, f) => {
    s"""
       |  const int P = ${p};
       |  const int C = ${c};
       |  const int F = ${f};
       |  srand(time(NULL));
       |  Buffer features = createBuffer(ctx, F * P * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer clusters = createBuffer(ctx, C * F * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer output = createBuffer(ctx, P * sizeof(float), HOST_READ | DEVICE_WRITE);
       |
       |  float* in_features = hostBufferSync(ctx, features, F * P * sizeof(float), HOST_WRITE);
       |  for (int i = 0; i < F * P ; i++) {
       |      in_features[i] = (float)(i+1);
       |  }
       |
       |  float* in_clusters = hostBufferSync(ctx, clusters, C * F * sizeof(float), HOST_WRITE);
       |    for (int i = 0; i < C * F ; i++) {
       |        in_features[i] = (float)(i+1);
       |    }
       |
       |  deviceBufferSync(ctx, features, F * P * sizeof(float), DEVICE_READ);
       |  deviceBufferSync(ctx, clusters, C * F * sizeof(float), DEVICE_READ);
       |""".stripMargin
  }
  val compute =
    s"""
       |    fun_init_run(ctx, output, P, C, F, features, clusters);
       |
       |""".stripMargin

  val finish =
    s"""
       |  // could add error checking
       |  destroyBuffer(ctx, features);
       |  destroyBuffer(ctx, clusters);
       |  destroyBuffer(ctx, output);
       |""".stripMargin
  // scalastyle:on


  test("run kmeans autotuning") {

    val inputSize: Int = 1 << 19

    // expert configuration
    val expertConfiguration: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (256: Nat),
      TuningParameter("ls1") -> (1: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("gs1") -> (1: Nat),
    )

    val defaultConfiguration: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (1: Nat),
      TuningParameter("ls1") -> (1: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("gs1") -> (1: Nat),
    )

    val configs = Seq(
      s"autotuning/config/kmeans/${inputSize.toString}/rs_cot_${inputSize.toString}.json",
      s"autotuning/config/kmeans/${inputSize.toString}/rs_emb_${inputSize.toString}.json",
      s"autotuning/config/kmeans/${inputSize.toString}/bolog_cot_${inputSize.toString}.json",
      s"autotuning/config/kmeans/${inputSize.toString}/atf_emb_${inputSize.toString}.json",
      s"autotuning/config/kmeans/${inputSize.toString}/ytoptccs_${inputSize.toString}.json",
    )

    runExperiment(
      name = s"K-means_GPU",
      configFiles = configs,
      iterations = 30,
      output = "artifact/results/rise/K-means_GPU",
      e = kmeans,
      hostCode = HostCode(init(inputSize, 10, 34), compute, finish),
      inputSizes = Seq(inputSize, 10, 34),
      plotOnly = false,
      expert = Some(expertConfiguration),
      default = Some(defaultConfiguration)
    )
  }
}
