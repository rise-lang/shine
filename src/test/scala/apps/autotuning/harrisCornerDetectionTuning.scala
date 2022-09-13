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

class harrisCornerDetectionTuning extends test_util.Tests {

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
       |""".stripMargin
  }

  val compute =
    s"""
       |fun_run(ctx, &fun, output, Ho, Wo, input);
       |""".stripMargin

  val finish =
    s"""
       |// TODO: could check output here
       |
       |destroyBuffer(ctx, input);
       |destroyBuffer(ctx, output);
       |""".stripMargin


  // helper
  def lowerOCL(e: ToBeTyped[Expr]): Expr =
    rewrite.ocl.unrollDots(util.printTime("infer", e.toExpr)).get

  ignore("execute harris") {
    // expression
    val tileX = 8
    val tileY = 8

    val harris =
      lowerOCL(
        ocl.harrisTileShiftInwardsPar(tileX, tileY, mapGlobal(_),
          ocl.harrisVecUnaligned2(4, _ => mapSeq, toPrivate)))
    println("harris: \n " + harris)

    // generate opencl kernel for executor
    val kernel = gen.opencl.kernel("harris").fromExpr(harris)
    println("kernel: \n" + translationToString(kernel))

    // generate code using hostcode-generation
    val harrisOCL = wrapOclRun(LocalSize(1, 1), GlobalSize(16, 16))(harris)
    val kernelHosted = gen.opencl.hosted.fromExpr(harrisOCL)
    println("kernelHosted: \n" + translateToString(kernelHosted))

    // execute kernel
    val result = autotune.execution.execute(
      expression = harrisOCL,
      hostCode = HostCode(init(128, 256), compute, finish),
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 10,
      speedupFactor = 100,
      execution = Median
    )
    println("result: " + result)
  }

  ignore("harris tuning ") {
    // expression
    //    val tileX = 8
    //    val tileY = 8

    val harrisTuning =
      tuningParam("tileX", RangeAdd(1, 256, 2), (tileX: Nat) =>
        tuningParam("tileY", RangeAdd(1, 256, 2), (tileY: Nat) =>
          tuningParam("vec", RangeAdd(1, 256, 2), (vec: Nat) =>
            lowerOCL(
              ocl.harrisTileShiftInwardsPar(tileX, tileY, mapGlobal(_),
                ocl.harrisVecUnaligned2(vec, _ => mapSeq, toPrivate)))
          )))

    val harrisOCLTuning =
      tuningParam("gs0", RangeMul(1, 256, 2), (gs0: Nat) =>
        tuningParam("gs1", RangeMul(1, 256, 2), (gs1: Nat) =>
          tuningParam("ls0", RangeMul(1, 256, 2), (ls0: Nat) =>
            tuningParam("ls1", RangeMul(1, 256, 2), (ls1: Nat) =>
              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(harrisTuning)
            ))))

    // start auto tuning

    val tuner = Tuner(
      hostCode = HostCode(init(128, 256), compute, finish),
      inputSizes = Seq(128, 256),
      samples = 100,
      name = "harris",
      output = "autotuning/harris",
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = Some("/home/jo/development/rise-lang/shine/autotuning/harris/harris2.json"),
      hmConstraints = true
    )

    val result = autotune.search(tuner)(harrisOCLTuning)
    val best = autotune.getBest(result.samples)
    println("result: \n" + result)
    println("best: \n" + best)
  }

  ignore("run harris autotuning mapglobal/seq 128 ") {

    val harrisTuning =
      tuningParam("tileX", RangeAdd(1, 256, 2), (tileX: Nat) =>
        tuningParam("tileY", RangeAdd(1, 256, 2), (tileY: Nat) =>
          tuningParam("vec", RangeAdd(1, 256, 2), (vec: Nat) =>
            lowerOCL(
              ocl.harrisTileShiftInwardsPar(tileX, tileY, mapWorkGroup(_),
                ocl.harrisVecUnaligned2(vec, mapLocal(_), toLocal)))
          )))

    //    val harrisTuning =
    //      tuningParam("tileX", RangeAdd(1, 256, 2), (tileX: Nat) =>
    //        tuningParam("tileY", RangeAdd(1, 256, 2), (tileY: Nat) =>
    //          tuningParam("vec", RangeAdd(1, 256, 2), (vec: Nat) =>
    //            lowerOCL(
    //              ocl.harrisTileShiftInwardsPar(tileX, tileY, mapGlobal(_),
    //                ocl.harrisVecUnaligned2(vec, _ => mapSeq, toPrivate)))
    //          )))

    val harrisOCLTuning =
      tuningParam("gs0", RangeMul(1, 256, 2), (gs0: Nat) =>
        tuningParam("gs1", RangeMul(1, 256, 2), (gs1: Nat) =>
          tuningParam("ls0", RangeMul(1, 256, 2), (ls0: Nat) =>
            tuningParam("ls1", RangeMul(1, 256, 2), (ls1: Nat) =>
              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(harrisTuning)
            ))))

    //    expert configuration
    val defaultConfiguration: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (16: Nat),
      TuningParameter("ls1") -> (32: Nat),
      TuningParameter("gs0") -> (128: Nat),
      TuningParameter("gs1") -> (256: Nat),
      TuningParameter("tileX") -> (16: Nat),
      TuningParameter("tileY") -> (32: Nat),
      TuningParameter("vec") -> (4: Nat)
    )

    // Michel
    val expertConfiguration: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (16: Nat),
      TuningParameter("ls1") -> (16: Nat),
      TuningParameter("gs0") -> (256: Nat),
      TuningParameter("gs1") -> (256: Nat),
      TuningParameter("tileX") -> (16: Nat),
      TuningParameter("tileY") -> (16: Nat),
      TuningParameter("vec") -> (4: Nat)
    )
    //
    //    val defaultConfiguration: Map[Nat, Nat] = Map(
    //      TuningParameter("ls0") -> (16: Nat),
    //      TuningParameter("ls1") -> (16: Nat),
    //      TuningParameter("gs0") -> (256: Nat),
    //      TuningParameter("gs1") -> (128: Nat),
    //      TuningParameter("tileX") -> (10: Nat),
    //      TuningParameter("tileY") -> (24: Nat),
    //      TuningParameter("vec") -> (2: Nat)
    //    )

    val configs = Seq(
      "autotuning/config/harris/128/rs_cot_128.json",
      "autotuning/config/harris/128/rs_emb_128.json",
      "autotuning/config/harris/128/bo_cot_128.json",
      "autotuning/config/harris/128/bounlog_cot_128.json",
      "autotuning/config/harris/128/atf_emb_128.json",
      "autotuning/config/harris/128/ytopt_128.json",
      "autotuning/config/harris/128/ytoptunlog_128.json"
    )

    runExperiment(
      name = "harris_128",
      configFiles = configs,
      iterations = 10,
      //      "experiment/results/harris",
      output = s"experiment/results/harris_128",
      harrisOCLTuning,
      HostCode(init(128, 256), compute, finish),
      inputSizes = Seq(128, 256),
      plotOnly = false,
      expert = Some(expertConfiguration),
      default = Some(defaultConfiguration)
    )
  }

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
      //      "autotuning/config/harris/1024/rs_cot_524288.json",
      //      "autotuning/config/harris/1024/rs_emb_524288.json",
      //      "autotuning/config/harris/1024/ls_cot_1024.json",
      //      "autotuning/config/harris/1024/bolog_cot_1024.json",
      "autotuning/config/harris/1024/bo_cot_1024.json",
      //      "autotuning/config/harris/1024/atf_emb_524288.json",
      //      "autotuning/config/harris/1024/ytoptlog_1024.json",
      //      "autotuning/config/harris/1024/ytopt_1024.json"
    )

    runExperiment(
      name = "harris_1024",
      configFiles = configs,
      iterations = 10,
      output = "experiment/results/harris_1024",
      harrisOCLTuning,
      HostCode(init(1024, 1024), compute, finish),
      inputSizes = Seq(1024, 1024),
      plotOnly = false,
      expert = Some(expertConfiguration),
      default = Some(defaultConfiguration)
    )
  }


  ignore("run harris autotuning mapGlobal/mapSeq 128 ") {

    val harrisTuning =
      tuningParam("tileX", RangeAdd(1, 256, 2), (tileX: Nat) =>
        tuningParam("tileY", RangeAdd(1, 256, 2), (tileY: Nat) =>
          tuningParam("vec", RangeAdd(1, 256, 2), (vec: Nat) =>
            lowerOCL(
              ocl.harrisTileShiftInwardsPar(tileX, tileY, mapGlobal(_),
                ocl.harrisVecUnaligned2(vec, _ => mapSeq, toPrivate)))
          )))

    val harrisOCLTuning =
      tuningParam("gs0", RangeMul(1, 256, 2), (gs0: Nat) =>
        tuningParam("gs1", RangeMul(1, 256, 2), (gs1: Nat) =>
          tuningParam("ls0", RangeMul(1, 256, 2), (ls0: Nat) =>
            tuningParam("ls1", RangeMul(1, 256, 2), (ls1: Nat) =>
              wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(harrisTuning)
            ))))

    // expert configuration
    val defaultConfiguration: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (16: Nat),
      TuningParameter("ls1") -> (32: Nat),
      TuningParameter("gs0") -> (128: Nat),
      TuningParameter("gs1") -> (256: Nat),
      TuningParameter("tileX") -> (16: Nat),
      TuningParameter("tileY") -> (32: Nat),
      TuningParameter("vec") -> (4: Nat)
    )

    val expertConfiguration: Map[Nat, Nat] = Map(
      TuningParameter("ls0") -> (8: Nat),
      TuningParameter("ls1") -> (16: Nat),
      TuningParameter("gs0") -> (128: Nat),
      TuningParameter("gs1") -> (256: Nat),
      TuningParameter("tileX") -> (8: Nat),
      TuningParameter("tileY") -> (16: Nat),
      TuningParameter("vec") -> (4: Nat)
    )

    val configs = Seq(
      "autotuning/config/harris/128/rs_cot_128.json",
      "autotuning/config/harris/128/rs_emb_128.json",
      "autotuning/config/harris/128/bo_cot_128.json",
      "autotuning/config/harris/128/bounlog_cot_128.json",
      "autotuning/config/harris/128/atf_emb_128.json",
      "autotuning/config/harris/128/ytopt_128.json",
      "autotuning/config/harris/128/ytoptunlog_128.json"
    )

    runExperiment(
      name = "harris_128",
      configFiles = configs,
      iterations = 10,
      //      "experiment/results/harris",
      output = s"experiment/results/harris_128",
      harrisOCLTuning,
      HostCode(init(128, 256), compute, finish),
      inputSizes = Seq(128, 256),
      plotOnly = false,
      expert = Some(expertConfiguration),
      //      default = Some(defaultConfiguration)
      default = None
    )
  }


  ignore("run harris autotuning mapGlobal/mapSeq 1024") {

    val harrisTuning =
      tuningParam("tileX", RangeAdd(1, 1024, 2), (tileX: Nat) =>
        tuningParam("tileY", RangeAdd(1, 1024, 2), (tileY: Nat) =>
          tuningParam("vec", RangeAdd(1, 1024, 2), (vec: Nat) =>
            lowerOCL(
              ocl.harrisTileShiftInwardsPar(tileX, tileY, mapGlobal(_),
                ocl.harrisVecUnaligned2(vec, _ => mapSeq, toPrivate)))
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
      TuningParameter("ls0") -> (32: Nat),
      TuningParameter("ls1") -> (32: Nat),
      TuningParameter("gs0") -> (1024: Nat),
      TuningParameter("gs1") -> (1024: Nat),
      TuningParameter("tileX") -> (32: Nat),
      TuningParameter("tileY") -> (32: Nat),
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
      "autotuning/config/harris/1024/rs_cot_524288.json",
      "autotuning/config/harris/1024/rs_emb_524288.json",
      "autotuning/config/harris/1024/bolog_cot_1024.json",
      "autotuning/config/harris/1024/bogplsp_cot_`1024.json",
      "autotuning/config/harris/1024/atf_emb_524288.json"
    )

    runExperiment(
      name = "harris_1024",
      configFiles = configs,
      iterations = 10,
      output = "experiment/results/harris_gs_1024",
      harrisOCLTuning,
      HostCode(init(1024, 1024), compute, finish),
      inputSizes = Seq(1024, 1024),
      plotOnly = false,
      expert = Some(expertConfiguration),
      default = Some(defaultConfiguration)
    )
  }

  ignore("search harris2 ") {

    // define expr

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

    val tuner = Tuner(
      hostCode = HostCode(init(1024, 1024), compute, finish),
      inputSizes = Seq(1024, 1024),
      samples = 100,
      name = "harris",
      output = "autotuning/harris",
      timeouts = Timeouts(5000, 5000, 5000),
      executionIterations = 10,
      speedupFactor = 100,
      configFile = Some("/home/jo/development/rise-lang/shine/autotuning/harris.json"),
      hmConstraints = true,
      saveToFile = true
    )

    val result = autotune.search(tuner)(harrisOCLTuning)
    val best = autotune.getBest(result.samples)
    println("result: \n" + result)
    println("best: \n" + best)


  }

}
