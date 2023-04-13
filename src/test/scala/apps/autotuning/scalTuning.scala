package apps.autotuning

import arithexpr.arithmetic.RangeMul
import rise.autotune
import rise.autotune._
import rise.core.DSL.HighLevelConstructs.{padCst2D, slide2D}
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Expr
import rise.core.primitives.{add, asScalar, asVectorAligned, join, mapSeq, split, vectorFromScalar}
import rise.core.types.DataType.{ArrayType, f32}
import rise.core.types.{AddressSpace, Nat, TuningParameter}
import rise.openCL.DSL.mapGlobal
import rise.openCL.primitives.oclReduceSeq
import shine.OpenCL.{GlobalSize, LocalSize}

class scalTuning extends test_util.Tests {

  import rise.openCL.DSL._

  val inputSize = 1 << 25

  val scal =
    tuningParam("s0", RangeMul(1, 1024, 2), (s0: Nat) =>
      tuningParam("s1", RangeMul(1, 1024, 2), (s1: Nat) =>
        depFun((n: Nat) => fun(ArrayType(n, f32))(input => fun(f32)(alpha =>
          input |>
            split(s0) |>
            mapWorkGroup(
              split(s1) >>
                mapLocal(mapSeq(fun(x => alpha * x))) >>
                join
            ) |> join
        )))
      ))


  val scalDefault =
    depFun((n: Nat) => fun(ArrayType(n, f32))(input => fun(f32)(alpha =>
      input |>
        split(1024) |>
        mapWorkGroup(
          split(4) >>
            mapLocal(mapSeq(fun(x => alpha * x))) >>
            join
        ) |> join
    )))


  val scalDefaultDefault =
    depFun((n: Nat) => fun(ArrayType(n, f32))(input => fun(f32)(alpha =>
      input |> mapGlobal(fun(x => alpha * x))
    )))


  val scalVec =
    tuningParam("s0", RangeMul(1, inputSize, 2), (s0: Nat) =>
      tuningParam("s1", RangeMul(1, inputSize, 2), (s1: Nat) =>
        tuningParam("vec", RangeMul(1, 1024, 2), (vec: Nat) =>
          depFun((n: Nat) => fun(n `.` f32)(input => fun(f32)(alpha =>
            input |>
              split(s0) |>
              mapWorkGroup(
                asVectorAligned(vec) >>
                  split(s1) >>
                  mapLocal(mapSeq(fun(x => vectorFromScalar(alpha) * x))) >>
                  join >> asScalar
              ) |>
              join
          )))
        )))

  val scalOcl: Expr =
    tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
      tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
        tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
          tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
            wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(scalVec)
          ))))

  // hostcode
  val init: Int => String = N => {
    s"""
       |const int N = ${N};
       |
       |srand(time(NULL));
       |
       |Buffer input = createBuffer(ctx, N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
       |Buffer output = createBuffer(ctx, N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_WRITE);
       |
       |float* m = hostBufferSync(ctx, input, N * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < N; i++) {
       |  // m[i] = (float)(rand()) * 10.0f;
       |  m[i] = 1.0f;
       |}
       |
       |int alpha = 10;
       |
       |// synchronize before entering timed section
       |deviceBufferSync(ctx, input, N * sizeof(float), DEVICE_READ);
       |waitFinished(ctx);
       |""".stripMargin
  }

  val compute =
    s"""
       |fun_run(ctx, &fun, output, N, input, alpha);
       |waitFinished(ctx);
       |""".stripMargin

  val finish =
    s"""
       |// TODO: could check output here
       |// use given gold expression?
       |
       |//float* outputScal = hostBufferSync(ctx, output, N * sizeof(float), HOST_READ);
       |//for(int i = 0; i < N; i++){
       | // printf("%f \\n", outputScal[i]);
       |//}
       |
       |destroyBuffer(ctx, input);
       |destroyBuffer(ctx, output);
       |""".stripMargin


  def executeStencilDefault(e: Expr) = {
    val inputSize: Int = 12

    println("Expression: \n" + e)

    val eOcl = wrapOclRun(LocalSize(1), GlobalSize(1024))(e)

    val result = rise.autotune.execution.execute(
      expression = eOcl,
      hostCode = HostCode(init(inputSize), compute, finish),
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 100,
      speedupFactor = 100,
      execution = Median
    )

    println("result: \n" + result)

  }

  ignore("test stencil execution") {
    executeStencilDefault(scalDefaultDefault)
  }

  ignore("scal tuning experiment") {
    val inputSize: Int = 1 << 25

    val tuner = Tuner(
      hostCode = HostCode(init(inputSize), compute, finish),
      inputSizes = Seq(inputSize),
      samples = 20, // defined by config file, value is ignored
      name = "scal",
      output = "autotuning",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = None,
      hmConstraints = true,
      runtimeStatistic = Minimum,
      saveToFile = true
    )

    autotune.search(tuner)(scalOcl)

  }


  test("run scal experiments") {
    val inputSize: Int = 1 << 25
    val inputSize2: Int = 1 << 25

    // expert configuration
    val expertConfiguration: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (256: Nat),
      TuningParameter("ls1") -> (1: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("gs1") -> (1: Nat),
      TuningParameter("s0") -> (512: Nat),
      TuningParameter("s1") -> (1: Nat),
      TuningParameter("vec") -> (2: Nat)
    )

    // expert configuration
    val defaultConfiguration: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (32: Nat),
      TuningParameter("ls1") -> (1: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("gs1") -> (1: Nat),
      TuningParameter("s0") -> (32: Nat),
      TuningParameter("s1") -> (32: Nat),
      TuningParameter("vec") -> (8: Nat)
    )

    val configs = Seq(
      s"autotuning/config/scal/${inputSize.toString}/rs_cot_${inputSize.toString}.json",
      s"autotuning/config/scal/${inputSize.toString}/rs_emb_${inputSize.toString}.json",
      s"autotuning/config/scal/${inputSize.toString}/bolog_cot_${inputSize.toString}.json",
      s"autotuning/config/scal/${inputSize.toString}/atf_emb_${inputSize.toString}.json",
      s"autotuning/config/scal/${inputSize.toString}/ytoptccs_${inputSize.toString}.json"
    )

    runExperiment(
      name = s"scal_${inputSize}",
      configFiles = configs,
      iterations = 30,
      output = s"experiment/results/paper/scal_${inputSize}",
      e = scalOcl,
      hostCode = HostCode(init(inputSize2), compute, finish),
      inputSizes = Seq(inputSize2),
      expert = Some(expertConfiguration),
      default = Some(defaultConfiguration),
      disableChecking = true
    )
  }
}
