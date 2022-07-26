package apps.autotuning

import arithexpr.arithmetic.{RangeMul, SteppedCase}
import rise.core.DSL.HighLevelConstructs._
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core.Expr
import rise.core.primitives._
import rise.core.types.DataType._
import rise.core.types._
import rise.openCL.DSL._
import rise.openCL.primitives.oclReduceSeq
import shine.OpenCL.{GlobalSize, KernelExecutor, LocalSize}
import util.Time.ms
import util.gen.c.function
import util.{Display, TimeSpan, gen}
import apps.convolution.hosted.blurYTiled2DTiledLoadingTransposedTuning
import arithexpr.arithmetic.SteppedCase
import rise.autotune
import rise.autotune._
import rise.core.DSL.HighLevelConstructs.{padCst2D, slide2D}
import rise.core.DSL.Type.n2nFun
import rise.core.DSL._
import rise.core.Expr
import rise.core.primitives.{add, depMapSeq, join, partition}
import rise.core.types.DataType.{ArrayType, f32}
import rise.core.types.{AddressSpace, Nat}
import rise.openCL.DSL.mapGlobal
import rise.openCL.primitives.oclReduceSeq
import shine.OpenCL.{GlobalSize, LocalSize}

class stencilTuning extends test_util.Tests {

  //  val inputSize = 1024
  val stencilSize = 11
  val padSize: Int = stencilSize / 2

  val basicStencil: Expr = {
    depFun((n: Nat) => fun(ArrayType(n, ArrayType(n, f32)))(input =>
      input |>
        padCst2D(padSize)(lf32(0.0f)) |>
        slide2D(stencilSize, 1) |>
        mapGlobal(1)(mapGlobal(0)(fun(nbh =>
          join(nbh) |> oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f))
        )))
    ))
  }
  //
  //  val partitionedStencil: Expr = {
  //    depFun((n: Nat) => fun(ArrayType(n, ArrayType(n, f32)))(input =>
  //      input |>
  //        padCst2D(padSize)(lf32(0.0f)) |>
  //        slide2D(stencilSize, 1) |>
  //        // partition2D(padSize, N - 2*padSize + ((1 + stencilSize) % 2)) :>>
  //        partition(3)(n2nFun(m =>
  //          SteppedCase(m, Seq(padSize, n - 2 * padSize, padSize))
  //        )) |>
  //        depMapSeq(
  //          // mapGlobal(0)(depMapSeqUnroll(mapGlobal(1)(join() >>> reduceSeq(add, 0.0f))))
  //          mapGlobal(1)(mapGlobal(0)(
  //            join >> oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f))
  //          ))
  //        ) |>
  //        join
  //    ))
  //  }
  //
  //
  //  val stencil2: Expr =
  //    tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
  //      tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
  //        tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
  //          tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
  //            wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(partitionedStencil)
  //          ))))

  val stencil: Expr =
    tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
      tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
        tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
          tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
            wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(basicStencil)
          ))))


  // hostcode
  val init: Int => String = N => {
    s"""
       |const int N = ${N};
       |
       |srand(time(NULL));
       |
       |Buffer input = createBuffer(ctx, N * N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
       |Buffer output = createBuffer(ctx, N * N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_WRITE);
       |
       |float* m = hostBufferSync(ctx, input, N * N * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < N * N; i++) {
       |  m[i] = (float)(rand())/(float)(RAND_MAX) * 10.0f;
       |}
       |
       |// synchronize before entering timed section
       |deviceBufferSync(ctx, input, N * N * sizeof(float), DEVICE_READ);
       |waitFinished(ctx);
       |""".stripMargin
  }

  val compute =
    s"""
       |fun_run(ctx, &fun, output, N, input);
       |waitFinished(ctx);
       |""".stripMargin

  val finish =
    s"""
       |// TODO: could check output here
       |// use given gold expression?
       |
       |destroyBuffer(ctx, input);
       |destroyBuffer(ctx, output);
       |""".stripMargin


  def executeStencilDefault(e: Expr) = {
    val inputSize: Int = 256

    println("Expression: \n" + e)

    val eOcl = wrapOclRun(LocalSize(2), GlobalSize(1024))(e)
    //    val eOcl = e

    //    println("Expression: \n" + eOcl)

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
    executeStencilDefault(stencil)
  }

  ignore("stencil tuning experiment") {
    val inputSize: Int = 1024

    val tuner = Tuner(
      hostCode = HostCode(init(inputSize), compute, finish),
      inputSizes = Seq(inputSize),
      samples = 20, // defined by config file, value is ignored
      name = "stencil",
      output = "autotuning",
      timeouts = Timeouts(10000, 10000, 10000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = None,
      hmConstraints = true,
      runtimeStatistic = Minimum,
      saveToFile = true
    )

    autotune.search(tuner)(stencil)

  }


  test("tune stencil 1024") {
    val inputSize: Int = 1024


    // expert configuration
    val expertConfiguration: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (4: Nat),
      TuningParameter("ls1") -> (4: Nat),
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
      s"autotuning/config/stencil/${inputSize.toString}/rs_cot_${inputSize.toString}.json",
      s"autotuning/config/stencil/${inputSize.toString}/rs_emb_${inputSize.toString}.json",
      s"autotuning/config/stencil/${inputSize.toString}/bogp_cot_${inputSize.toString}.json",
      s"autotuning/config/stencil/${inputSize.toString}/bogplsp_cot_${inputSize.toString}.json",
      s"autotuning/config/stencil/${inputSize.toString}/atf_emb_${inputSize.toString}.json"
    )

    runExperiment(
      name = s"stencil_${inputSize}",
      configFiles = configs,
      iterations = 10,
      //      output = s"experiment/results/stencil_${inputSize}",
      output = s"/home/jo/development/experiments/tuning/results/stencil_${inputSize}",
      stencil,
      HostCode(init(inputSize), compute, finish),
      Seq(inputSize),
      plotOnly = true,
      expert = Some(expertConfiguration),
      default = Some(defaultConfiguration)
    )
  }

}
