package apps.autotuning

import arithexpr.arithmetic.RangeMul
import rise.autotune
import rise.autotune.{HostCode, Median, Minimum, Timeouts, Tuner, tuningParam, wrapOclRun}
import rise.core._
import rise.core.types.{Nat, _}
import shine.OpenCL.{GlobalSize, LocalSize}

class kmeansTuning extends test_util.Tests {

  // todo adjust this for auto-tuning
  // kmeans using loacl memory implementation from lift
  //
  //  @Test
  //  def kMeansLocalMemory(): Unit = {
  //
  //    val numPoints = 1024
  //    val numClusters = 5
  //    val numFeatures = 8
  //
  //    val points = Array.fill(numPoints, numFeatures)(util.Random.nextFloat())
  //    val clusters = Array.fill(numClusters, numFeatures)(util.Random.nextFloat())
  //
  //    val gold = calculateMembership(points, clusters)
  //
  //    val splitFactor = 128
  //
  //    val kMeans = fun(
  //      featuresType, clustersType,
  //      (features, clusters) => {
  //        features :>> Transpose() :>> Split(splitFactor) :>> MapWrg(\(featuresChunk =>
  //          clusters :>> toLocal(MapLcl(MapSeq(id))) :>> Let(localClusters =>
  //            MapLcl(\(feature => {
  //              localClusters :>> ReduceSeq(\((tuple, cluster) => {
  //
  //                val dist = Zip(feature, cluster) :>> ReduceSeq(\((acc, b) => update2(acc, Get(b, 0), Get(b, 1))), 0.0f)
  //                Zip(dist, tuple) :>> MapSeq(test)
  //
  //              }), Value("{3.40282347e+38, 0, 0}", ArrayTypeWSWC(TupleType(Float, Int, Int), 1))) :>>
  //                toGlobal(MapSeq(MapSeq(select)))
  //            })) $ featuresChunk
  //          )
  //        )) :>> Join()
  //      })
  //
  //    val (output, _) = Execute(numPoints)[Array[Int]](kMeans, points.transpose, clusters)
  //    assertArrayEquals(gold, output)
  //  }


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
       |// init checking
       |  FILE *fptr;
       |
       |  if ((fptr = fopen("autotuning/gold/kmeans_${f}_${c}_${f}.csv","r")) == NULL){
       |    return 133;
       |   }
       |  float gold[P];
       |
       |  for(int i = 0; i<P; i++){
       |    fscanf(fptr, "%f,", &gold[i]);
       |  }
       |  fclose(fptr);
       |
       |  deviceBufferSync(ctx, features, F * P * sizeof(float), DEVICE_READ);
       |  deviceBufferSync(ctx, clusters, C * F * sizeof(float), DEVICE_READ);
       |""".stripMargin
  }
  val compute =
    s"""
       |    fun_init_run(ctx, output, P, C, F, features, clusters);
       |
       |  float* out = hostBufferSync(ctx, output, P * sizeof(float), HOST_READ);
       |  for(int i = 0; i < P; i++){
       |    if(out[i] != gold[i]){
       |      return 132;
       |    }
       |  }
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



  test("create output file") {

    val P = 1024
    val C = 10
    val F = 34

    // compute kmeans
    val gold_expression = util.gen.c.function.asStringFromExpr(apps.kmeans.kmeansSeq)
    // wrap host code around

    // write C to file
    //    val result = C.mkString(",")
    //    util.writeToPath(s"autotuning/gold/kmeans_${P}_${C}_${F}.csv", result)

  }

  ignore("execute kmeans") {
    val params: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (32: Nat),
      TuningParameter("ls1") -> (1: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("gs1") -> (1: Nat)
    )

    val kmeans_replaced = rise.core.substitute.natsInExpr(params, kmeans)

    val result = autotune.execution.execute(
      expression = kmeans_replaced,
      hostCode = HostCode(init(1024, 5, 34), compute, finish),
      timeouts = Timeouts(10000, 10000, 100000),
      executionIterations = 10,
      speedupFactor = 100,
      execution = Median
    )

    println("result: " + result)
  }

  ignore("search kmeans with generated config file") {

    val tuner = Tuner(

      hostCode = HostCode(init(1024, 5, 34), compute, finish),
      inputSizes = Seq(1024, 5, 34),
      samples = 10,
      name = "kmeans",
      output = "autotuning/kmeans",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      runtimeStatistic = Median,
      speedupFactor = 100,
      configFile = None,
      hmConstraints = false,
    )

    val tuningResult = autotune.search(tuner)(kmeans)

    println("tuningResult: \n")
    tuningResult.samples.foreach(elem => println(elem))

    val bestSample = autotune.getBest(tuningResult.samples)
    println("bestSample: \n" + bestSample)
    println("runtime: \n" + bestSample.get.runtime)
  }

  ignore("search kmeans with manual config file") {

    val tuner = Tuner(
      hostCode = HostCode(init(1024, 5, 34), compute, finish),
      inputSizes = Seq(1024, 5, 34),
      samples = 20,
      name = "kmeans",
      output = "autotuning/kmeans",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = Some("autotuning/config/kmeans/kmeans_exhaustive.json"),
      //      configFile = None,
      hmConstraints = true,
      runtimeStatistic = Minimum
    )

    val tuningResult = autotune.search(tuner)(kmeans)

    println("tuningResult: \n")
    tuningResult.samples.foreach(elem => println(elem))

    val bestSample = autotune.getBest(tuningResult.samples)
    println("bestSample: \n" + bestSample)
    println("runtime: \n" + bestSample.get.runtime)
  }

  def runExperiments(configFiles: Seq[String], iterations: Int) = {
    for (i <- 1 to iterations) {
      configFiles.foreach(runTuning)
    }
  }

  def runTuning(configFile: String) = {
    val version = rise.autotune.configFileGeneration.parseFromJson(configFile, "application_name")

    val tuner = Tuner(
      hostCode = HostCode(init(1024, 5, 34), compute, finish),
      inputSizes = Seq(1024, 5, 34),
      samples = 20, // defined by config file
      name = version,
      output = s"autotuning/kmeans/${version}",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = Some(configFile),
      hmConstraints = true,
      saveToFile = true
    )
    autotune.search(tuner)(kmeans)
  }

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
      TuningParameter("ls1") -> (16: Nat),
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
      name = s"kmeans_${inputSize}",
      configFiles = configs,
      iterations = 30,
      output = s"experiment/results/paper/kmeans_${inputSize}",
      e = kmeans,
      hostCode = HostCode(init(inputSize, 10, 34), compute, finish),
      inputSizes = Seq(inputSize, 10, 34),
      plotOnly = false,
      expert = Some(expertConfiguration),
      default = Some(defaultConfiguration)
    )
  }
}
