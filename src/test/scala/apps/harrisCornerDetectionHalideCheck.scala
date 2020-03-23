package apps

import harrisCornerDetectionHalide._
import apps.{harrisCornerDetectionHalideRewrite => rewrite}
import rise.core._
import util.gen

class harrisCornerDetectionHalideCheck
  extends shine.test_util.TestsWithExecutor
{
  test("harris typechecks") {
    val typed = util.printTime("infer", types.infer(harris))
    println(typed.t)
  }

  val H = 128
  val W = 64 // x vector width
  val Hi = H + 4
  val Wi = W + 4 // x vector width
  val strip = 32
  assert(H % strip == 0)

  def lowerOMP(e: Expr): Expr =
    rewrite.unrollDots(util.printTime("infer", types.infer(e)))

  def checkOMP(lowered: Expr): Unit = {
    val dumbLowering = lowerOMP(omp.harrisSeqWrite)
    val goldProg = gen.OpenMPProgram(dumbLowering, "harrisGold")

    val prog = util.printTime("codegen",
      gen.OpenMPProgram(lowered, "harris"))

    val testCode =
      s"""
         | #include <stdlib.h>
         | #include <stdio.h>
         | #include <math.h>
         |
         | ${goldProg.code}
         |
         | ${prog.code}
         |
         | int main(int argc, char** argv) {
         |   float* input = malloc(${3 * Hi * Wi * v} * sizeof(float));
         |   float* gold = malloc(${H * W * v} * sizeof(float));
         |   float* output = malloc(${H * W * v} * sizeof(float));
         |
         |   for (int i = 0; i < ${3 * Hi * Wi * v}; i++) {
         |     input[i] = (float)((i + 179) % 256) / 256.0f;
         |   }
         |
         |   ${goldProg.function.name}(gold, $H, $W, input);
         |   ${prog.function.name}(output, $H, $W, input);
         |
         |   int exit_status = 0;
         |   for (int i = 0; i < ${H * W * v}; i++) {
         |     if (fabs(gold[i] - output[i]) > 0.001) {
         |       fprintf(stderr, "%.4f != %.4f\\n", gold[i], output[i]);
         |       exit_status = 1;
         |       break;
         |     }
         |   }
         |
         |   free(input);
         |   free(gold);
         |   free(output);
         |   return exit_status;
         | }
         |""".stripMargin
    util.printTime("execute", util.Execute(testCode))
  }

  test("harrisBuffered generates valid OpenMP") {
    checkOMP(lowerOMP(omp.harrisBuffered))
  }

  import shine.OpenCL._

  def lowerOCL(e: Expr): Expr =
    rewrite.ocl.unrollDots(util.printTime("infer", types.infer(e)))

  def checkOCL(run: Array[Array[Array[Float]]]
    => (Array[Float], util.TimeSpan[util.Time.ms])
  ): Unit = {
    val dumbLowering = lowerOCL(ocl.harrisSeqWrite)
    val goldProg = gen.OpenCLKernel(dumbLowering, "harrisGold")

    val localSize = LocalSize(1)
    val globalSize = GlobalSize(1)

    val random = new scala.util.Random()
    val input = Array.fill(3, Hi, Wi * v)(random.nextFloat)

    val f = goldProg.as[ScalaFunction `(`
      Int `,` Int `,` Array[Array[Array[Float]]]
      `)=>` Array[Float]]
    val (gold, goldTime) = f(localSize, globalSize)(H `,` W `,` input)

    val (output, time) = run(input)

    println(s"gold time: $goldTime")
    println(s"time: $time")
    util.assertSame(output, gold, "output is different from gold")
  }

  test("harrisBuffered generates valid OpenCL") {
    val prog = util.printTime("codegen",
      gen.OpenCLKernel(lowerOCL(ocl.harrisBuffered), "harris"))

    val localSize = LocalSize(1)
    val globalSize = GlobalSize(1)
    val f = prog.as[ScalaFunction `(`
      Int `,` Int `,` Array[Array[Array[Float]]]
      `)=>` Array[Float]]

    checkOCL(input => f(localSize, globalSize)(H `,` W `,` input))
  }

  def checkOCLSeq(lowered: Expr): Unit = {
    val prog = util.printTime("codegen",
      gen.OpenCLKernel(lowered, "harris"))

    val localSize = LocalSize(1)
    val globalSize = GlobalSize(1)
    val f = prog.as[ScalaFunction `(`
      Int `,` Int `,` Array[Array[Array[Float]]]
      `)=>` Array[Float]]

    checkOCL(input => f(localSize, globalSize)(H `,` W `,` input))
  }

  test("harrisVecUnaligned generates valid OpenCL") {
    checkOCLSeq(lowerOCL(ocl.harrisVecUnaligned))
  }

  test("harrisBufferedVecUnaligned generates valid OpenCL") {
    checkOCLSeq(lowerOCL(ocl.harrisBufferedVecUnaligned))
  }

  test("harrisBufferedVecUnalignedSplitPar generates valid OpenCL") {
    val prog = util.printTime("codegen", gen.OpenCLKernel(
      lowerOCL(ocl.harrisBufferedVecUnalignedSplitPar), "harris"))

    val localSize = LocalSize(1)
    val globalSize = GlobalSize(H / 32)
    val f = prog.as[ScalaFunction `(`
      Int `,` Int `,` Array[Array[Array[Float]]]
      `)=>` Array[Float]]

    checkOCL(input => f(localSize, globalSize)(H `,` W `,` input))
  }

  test("harrisBufferedVecAligned generates valid OpenCL") {
    checkOCLSeq(lowerOCL(ocl.harrisBufferedVecAligned))
  }

  test("harrisBuffered rewrite generates valid OpenCL") {
    val typed = util.printTime("infer", types.infer(harris))
    val lowered = rewrite.ocl.harrisBuffered(typed)
    gen.OpenCLKernel(lowered)
    // TODO: checkOCLSeq(lowered)
  }

  test("harrisBufferedSplitPar rewrite generates valid OpenCL") {
    val typed = util.printTime("infer", types.infer(harris))
    val lowered = rewrite.ocl.harrisBufferedSplitPar(strip)(typed)
    gen.OpenCLKernel(lowered)
    // TODO: checkOCLSeq(lowered)
  }
}
