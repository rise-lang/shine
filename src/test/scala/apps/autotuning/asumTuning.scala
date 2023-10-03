package apps.autotuning

import shine.DPIA.Types.ExpType
import rise.elevate.Rise
import shine.OpenCL.{GlobalSize, LocalSize}
import rise.core._
import rise.core.types._
import rise.core.DSL._
import rise.core.primitives._
import Type._
import HighLevelConstructs.reorderWithStride
import rise.autotune
import rise.autotune.{HostCode, Median, Timeouts, tuningParam, wrapOclRun}
import util.{SyntaxChecker, gen}
import rise.elevate.rules.traversal.default._
import rise.openCL.DSL.{mapGlobal, mapLocal, mapWorkGroup, toGlobal, toLocalFun}
import rise.openCL.primitives.{oclIterate, oclReduceSeq}
import shine.OpenCL.KernelExecutor.KernelNoSizes.fromKernelModule
import util.gen.c.function
import apps.separableConvolution2D.mulT
import rise.core.DSL.HighLevelConstructs.reorderWithStride
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.primitives.{let => _, _}
import rise.core.types.DataType._
import rise.core.types._

import scala.util.Random
import apps.mv.ocl._
import arithexpr.arithmetic.RangeMul
import rise.autotune
import rise.autotune._
import rise.core.DSL.Type._
import rise.core.DSL._
import rise.core._
import rise.core.types._
import shine.OpenCL.{GlobalSize, LocalSize}
import apps.mm.mmNVIDIAWithParams
import arithexpr.arithmetic.{RangeAdd, RangeMul}
import elevate.core.strategies.traversal.topDown
import rise.elevate.Rise
import rise.elevate.rules.algorithmic.fuseReduceMap
import rise.elevate.strategies.traversal._

import scala.collection.immutable
import scala.language.postfixOps
import exploration.runner.CExecutor
import exploration.strategies.simpleStrategiesGPU
import rise.core.types.{Nat, TuningParameter}
import rise.elevate.rules.lowering.reduceOCL

import scala.language.postfixOps
import scala.sys.process._

class asumTuning extends test_util.Tests {

  val inputSize: Int = 2 << 25

  def inputT(n: Nat) = ArrayType(n, f32)

  val abs =
    depFun((t: DataType) => foreignFun("my_abs", Seq("y"), "{ return fabs(y); }", t ->: t))
  val fabs = abs(f32)
  val add = fun(x => fun(a => x + a))

  val nvidiaDerived1: ToBeTyped[Expr] = {
    tuningParam("sp0", RangeMul(1, inputSize, 2), (sp0: Nat) =>
      tuningParam("sp1", RangeMul(1, inputSize, 2), (sp1: Nat) =>
        tuningParam("stride", RangeMul(1, inputSize, 2), (stride: Nat) =>
          depFun((n: Nat) =>
            fun(inputT(n))(input =>
              input |>
                split(sp0) |>
                mapWorkGroup(
                  reorderWithStride(stride) >>
                    split(sp1) >>
                    mapLocal(
                      oclReduceSeq(AddressSpace.Private)(
                        fun(a => fun(x => abs(f32)(x) + a))
                      )(lf32(0.0f))
                    )
                ) |> join
            )
          ))))
  }

  val asum: Expr = {
    tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
      tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
        wrapOclRun(LocalSize(ls0), GlobalSize(gs0))(nvidiaDerived1)))
  }

  // scalastyle:off
  val init: (Int) => String = (N) => {
    s"""
       |  const int N = ${N};
       |  srand(time(NULL));
       |  Buffer input = createBuffer(ctx, N * sizeof(float), HOST_WRITE | DEVICE_READ);
       |  Buffer output = createBuffer(ctx, 1 * sizeof(float), HOST_READ | DEVICE_WRITE);
       |
       |  float* in = hostBufferSync(ctx, input, N * sizeof(float), HOST_WRITE);
       |  for (int i = 0; i < N ; i++) {
       |    in[i] = (float)((i % 100) + 1);
       |  }
       |
       |  deviceBufferSync(ctx, input, N * sizeof(float), DEVICE_READ);
       |""".stripMargin
  }
  val compute =
    s"""
       |  fun_init_run(ctx, output, N, input);
       |
       |""".stripMargin

  val finish =
    s"""
       |  destroyBuffer(ctx, input);
       |  destroyBuffer(ctx, output);
       |""".stripMargin
  // scalastyle:on

  test("tune asum") {
    val inputSize: Int = 2 << 23

    // expert configuration
    val expertConfiguration: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (128: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("sp0") -> ((2048 * 128): Nat),
      TuningParameter("sp1") -> (2048: Nat),
      TuningParameter("stride") -> (128: Nat),
    )

    // todo adjust this
    val defaultConfiguration: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (4: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("sp0") -> (16777216: Nat),
      TuningParameter("sp1") -> (512: Nat),
      TuningParameter("stride") -> (4096: Nat),
    )

    val configs = Seq(
      s"autotuning/config/asum/${inputSize.toString}/rs_cot_${inputSize.toString}.json",
      s"autotuning/config/asum/${inputSize.toString}/rs_emb_${inputSize.toString}.json",
      s"autotuning/config/asum/${inputSize.toString}/atf_emb_${inputSize.toString}.json",
      s"autotuning/config/asum/${inputSize.toString}/bolog_cot_${inputSize.toString}.json",
      s"autotuning/config/asum/${inputSize.toString}/ytoptccs_${inputSize.toString}.json",
    )

    runExperiment(
      name = s"Asum_GPU",
      configFiles = configs,
      iterations = 30,
      output = s"artifact/results/Asum_GPU",
      asum,
      HostCode(init(inputSize), compute, finish),
      Seq(inputSize),
      plotOnly = false,
      expert = Some(expertConfiguration),
      default = Some(defaultConfiguration)
    )
  }
}
