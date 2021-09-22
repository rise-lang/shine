package apps.autotuning

import apps.harrisCornerDetectionHalide.ocl
import rise.core.DSL.ToBeTyped
import rise.core.Expr
import rise.core.primitives.mapSeq
import rise.openCL.DSL.{mapGlobal, toPrivate}
import apps.{harrisCornerDetectionHalideRewrite => rewrite}
import arithexpr.arithmetic.ArithExpr.toInt
import rise.autotune
import rise.autotune.execution.{getRuntimeFromClap, logger}
import rise.autotune.{HostCode, Median, Timeouts, tuningParam, wrapOclRun}
import rise.core.types.Nat
import shine.OpenCL.{GlobalSize, LocalSize}
import shine.OpenCL.KernelModule.translationToString
import shine.OpenCL.Module.translateToString
import util.ExecuteOpenCL.{includes, libDirs, libs}
import util.gen

class harrisCornerDetectionTuning extends test_util.Tests {

  // hostcode

  val init: (Int, Int) => String = (Ho, Wo) => {
    s"""
       |const int Hi = ${Ho+4};
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

  test("execute harris"){
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

  test("harris tuning ") {
    // expression
    val tileX = 8
    val tileY = 8

    val harrisTuning =
      tuningParam("tileX", (tileX: Nat) =>
        tuningParam("tileY", (tileY: Nat) =>
          tuningParam("vec", (vec: Nat) =>
            lowerOCL(
              ocl.harrisTileShiftInwardsPar(tileX, tileY, mapGlobal(_),
                ocl.harrisVecUnaligned2(vec, _ => mapSeq, toPrivate)))
    )))

    val harrisOCLTuning =
      tuningParam("gs0", (gs0: Nat) =>
        tuningParam("gs1", (gs1: Nat) =>
          tuningParam("ls0", (ls0: Nat) =>
            tuningParam("ls1", (ls1: Nat) =>
                wrapOclRun(LocalSize(ls0, ls1), GlobalSize(gs0, gs1))(harrisTuning)
            ))))

    println("harrisOCLTuning: \n" + harrisOCLTuning)

    // start auto tuning

  }

}
