package apps.autotuning

import apps.harrisCornerDetectionHalide.ocl
import rise.core.DSL.ToBeTyped
import rise.core.Expr
import rise.core.primitives.mapSeq
import rise.openCL.DSL.{mapGlobal, mapLocal, mapWorkGroup, toLocal, toPrivate}
import apps.{harrisCornerDetectionHalideRewrite => rewrite}
import arithexpr.arithmetic.ArithExpr.toInt
import arithexpr.arithmetic.{RangeAdd, RangeMul}
import rise.autotune
import rise.autotune.execution.{getRuntimeFromClap, logger}
import rise.autotune.{HostCode, Median, Minimum, Timeouts, Tuner, tuningParam, wrapOclRun}
import rise.core.types.{Nat, TuningParameter}
import shine.OpenCL.{GlobalSize, LocalSize}
import shine.OpenCL.KernelModule.translationToString
import shine.OpenCL.Module.translateToString
import util.ExecuteOpenCL.{includes, libDirs, libs}
import util.gen
import util.gen.c.function

class harrisTuning extends test_util.Tests {

  // hostcode
  val init: (Int, Int) => String = (Ho, Wo) => {
    s"""
       |const int Hi = ${Ho + 4};
       |const int Wi = ${Wo};
       |const int Ho = ${Ho};
       |const int Wo = ${Wo};
       |
       |srand(time(NULL));
       |
       |Buffer input = createBuffer(ctx, 3 * Hi * Wi * sizeof(float), HOST_WRITE | DEVICE_READ);
       |Buffer output = createBuffer(ctx, Ho * Wo *  sizeof(float), HOST_READ | DEVICE_WRITE);
       |
       |float* in = hostBufferSync(ctx, input, 3 * Hi * Wi * sizeof(float), HOST_WRITE);
       |for (int i = 0; i < 3 * Hi * Wi; i++) {
       |  in[i] = (float)((i + 179) % 256) / 25.6f;
       |}
       |
       |""".stripMargin
  }

  val compute =
    s"""
       |fun_run(ctx, &fun, output, Ho, Wo, input);
       |""".stripMargin

  val finish =
    s"""
       |
       |destroyBuffer(ctx, input);
       |destroyBuffer(ctx, output);
       |""".stripMargin

  // helper
  def lowerOCL(e: ToBeTyped[Expr]): Expr =
    rewrite.ocl.unrollDots(util.printTime("infer", e.toExpr)).get

  test("run harris autotuning mapLocal/mapWorkGroup 1024") {

    val harrisTuning =
      tuningParam("tileX", RangeAdd(1, 1024, 2), (tileX: Nat) =>
        tuningParam("tileY", RangeAdd(1, 1024, 2), (tileY: Nat) =>
          tuningParam("vec", RangeAdd(1, 1024, 2), (vec: Nat) =>
            lowerOCL(
              ocl.harrisTileShiftInwardsPar(tileX, tileY, mapWorkGroup(_),
                ocl.harrisVecUnaligned2(vec, mapLocal(_), toLocal)))
          )))

    val harrisOCLTuning =
      tuningParam("gs0", RangeMul(1, 1024, 2), (gs0: Nat) =>
        tuningParam("gs1", RangeMul(1, 1024, 2), (gs1: Nat) =>
          tuningParam("ls0", RangeMul(1, 1024, 2), (ls0: Nat) =>
            tuningParam("ls1", RangeMul(1, 1024, 2), (ls1: Nat) =>
              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(harrisTuning)
            ))))


    // expert configuration
    val expertConfiguration: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (8: Nat),
      TuningParameter("ls1") -> (32: Nat),
      TuningParameter("gs0") -> (128: Nat),
      TuningParameter("gs1") -> (1024: Nat),
      TuningParameter("tileX") -> (64: Nat),
      TuningParameter("tileY") -> (16: Nat),
      TuningParameter("vec") -> (4: Nat)
    )

    val defaultConfiguration: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (1: Nat),
      TuningParameter("ls1") -> (1: Nat),
      TuningParameter("gs0") -> (1: Nat),
      TuningParameter("gs1") -> (1: Nat),
      TuningParameter("tileX") -> (1: Nat),
      TuningParameter("tileY") -> (1: Nat),
      TuningParameter("vec") -> (4: Nat)
    )

    val configs = Seq(
      "autotuning/config/harris/1024/rs_cot_1024.json",
      "autotuning/config/harris/1024/rs_emb_1024.json",
      "autotuning/config/harris/1024/bolog_cot_1024.json",
      "autotuning/config/harris/1024/atf_emb_1024.json",
      "autotuning/config/harris/1024/ytoptccs_1024.json"
    )

    runExperiment(
      name = "Harris_GPU",
      configFiles = configs,
      iterations = 30,
      output = "artifact/results/rise/Harris_GPU",
      harrisOCLTuning,
      HostCode(init(1024, 1024), compute, finish),
      inputSizes = Seq(1024, 1024),
      plotOnly = false,
      expert = Some(expertConfiguration),
      default = Some(defaultConfiguration)
    )
  }
}
