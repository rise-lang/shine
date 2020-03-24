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

  val Ho = 128
  val Wo = 256
  val Wov = Wo - 4 // valid extent in a Wo stride
  val Hi = Ho + 4
  val Wi = Wo

  val strip = 32
  assert(Ho % strip == 0)

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
         |   float* input = malloc(${3 * Hi * Wi} * sizeof(float));
         |   float* gold = malloc(${Ho * Wo} * sizeof(float));
         |   float* output = malloc(${Ho * Wo} * sizeof(float));
         |
         |   for (int i = 0; i < ${3 * Hi * Wi}; i++) {
         |     input[i] = (float)((i + 179) % 256) / 25.6f;
         |   }
         |
         |   ${goldProg.function.name}(gold, $Ho, $Wo, input);
         |   ${prog.function.name}(output, $Ho, $Wo, input);
         |
         |   int exit_status = 0;
         |   for (int y = 0; y < $Ho; y++) {
         |     for (int x = 0; x < $Wov; x++) {
         |       int i = y * $Wo + x;
         |       if (fabs(gold[i] - output[i]) > 0.001) {
         |         fprintf(stderr, "%.4f != %.4f\\n", gold[i], output[i]);
         |         exit_status = 1;
         |         break;
         |       }
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

  def checkOCL(lowered: Expr, ls: LocalSize, gs: GlobalSize): Unit = {
    assert(lowered.t == types.infer(harris).t)
    val prog = util.printTime("codegen",
      gen.OpenCLKernel(lowered, "harris"))

    val dumbLowering = lowerOCL(ocl.harrisSeqWrite)
    val goldProg = gen.OpenCLKernel(dumbLowering, "harrisGold")

    val random = new scala.util.Random()
    val input = Array.fill(3, Hi, Wi)(random.nextFloat * 10.0f)

    val fg = goldProg.as[ScalaFunction `(`
      Int `,` Int `,` Array[Array[Array[Float]]]
      `)=>` Array[Float]]
    val (gold, goldTime) = fg(LocalSize(1), GlobalSize(1))(Ho `,` Wo `,` input)

    val f = prog.as[ScalaFunction `(`
      Int `,` Int `,` Array[Array[Array[Float]]]
      `)=>` Array[Float]]
    val (output, time) = f(ls, gs)(Ho `,` Wo `,` input)

    println(s"gold time: $goldTime")
    println(s"time: $time")
    println(output.sliding(Wov, Wo).next.mkString(","))
    println("--")
    println(gold.sliding(Wov, Wo).next.mkString(","))
    println("--")
    util.assertSame(
      output.sliding(Wov, Wo).toArray,
      gold.sliding(Wov, Wo).toArray,
      "output is different from gold")
  }

  test("harrisBuffered generates valid OpenCL") {
    checkOCL(lowerOCL(ocl.harrisBuffered), LocalSize(1), GlobalSize(1))
  }

  test("harrisVecUnaligned generates valid OpenCL") {
    assert(Wo % 4 == 0)
    checkOCL(lowerOCL(ocl.harrisVecUnaligned(4)), LocalSize(1), GlobalSize(1))
  }

  test("harrisBufferedVecUnaligned generates valid OpenCL") {
    assert(Wo % 4 == 0)
    checkOCL(lowerOCL(ocl.harrisBufferedVecUnaligned(4)),
      LocalSize(1), GlobalSize(1))
  }

  test("harrisBufferedVecUnalignedSplitPar(4) generates valid OpenCL") {
    assert(Wo % 4 == 0)
    checkOCL(lowerOCL(ocl.harrisSplitPar(strip, 4,
      ocl.harrisBufferedVecUnaligned(4))),
      LocalSize(1), GlobalSize(Ho / strip))
  }

  test("harrisBufferedVecUnalignedSplitPar(8) generates valid OpenCL") {
    assert(Wo % 8 == 0)
    checkOCL(lowerOCL(ocl.harrisSplitPar(strip, 8,
      ocl.harrisBufferedVecUnaligned(8))),
      LocalSize(1), GlobalSize(Ho / strip))
  }

  test("harrisBufferedVecAligned generates valid OpenCL") {
    assert(Wo % 4 == 0)
    checkOCL(lowerOCL(ocl.harrisBufferedVecAligned(4)),
      LocalSize(1), GlobalSize(1))
  }

  test("harrisBufferedVecAlignedSplitPar(4) generates valid OpenCL") {
    assert(Wo % 4 == 0)
    checkOCL(lowerOCL(ocl.harrisSplitPar(strip, 4,
      ocl.harrisBufferedVecAligned(4))),
      LocalSize(1), GlobalSize(Ho / strip))
  }

  test("harrisBufferedVecAlignedSplitPar(8) generates valid OpenCL") {
    assert(Wo % 8 == 0)
    checkOCL(lowerOCL(ocl.harrisSplitPar(strip, 8,
      ocl.harrisBufferedVecAligned(8))),
      LocalSize(1), GlobalSize(Ho / strip))
  }

  test("harrisBuffered rewrite generates valid OpenCL") {
    val typed = util.printTime("infer", types.infer(harris))
    val lowered = rewrite.ocl.harrisBuffered(typed)
    checkOCL(lowered, LocalSize(1), GlobalSize(1))
  }

  test("harrisBufferedSplitPar rewrite generates valid OpenCL") {
    val typed = util.printTime("infer", types.infer(harris))
    val lowered = rewrite.ocl.harrisBufferedSplitPar(strip)(typed)
    checkOCL(lowered, LocalSize(1), GlobalSize(Ho / strip))
  }

  test("harrisBufferedVecUnalignedSplitPar rewrite generates valid OpenCL") {
    val typed = util.printTime("infer", types.infer(harris))
    val lowered = rewrite.ocl.harrisBufferedVecUnalignedSplitPar(v, strip)(typed)
    checkOCL(lowered, LocalSize(1), GlobalSize(Ho / strip))
  }
}
