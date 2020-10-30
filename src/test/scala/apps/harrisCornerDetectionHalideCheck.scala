package apps

import apps.harrisCornerDetectionHalide._
import apps.{harrisCornerDetectionHalideRewrite => rewrite}
import rise.core.DSL.ToBeTyped
import rise.core._
import util.gen

class harrisCornerDetectionHalideCheck
  extends test_util.TestsWithExecutor
{
  test("harris typechecks") {
    val typed = util.printTime("infer", harris(1, 1).toExpr)
    println(typed.t)
  }

  val Ho = 128
  val Wo = 256
  val Wov = Wo - 4 // valid extent in a Wo stride
  val Hi = Ho + 4
  val Wi = Wo

  val strip = 32
  assert(Ho % strip == 0)

  def lowerOMP(e: ToBeTyped[Expr]): Expr =
    rewrite.unrollDots(util.printTime("infer", e.toExpr)).get

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

  def lowerOCL(e: ToBeTyped[Expr]): Expr =
    rewrite.ocl.unrollDots(util.printTime("infer", e.toExpr)).get

  def checkOCL(lowered: Expr, ls: LocalSize, gs: GlobalSize): Unit = {
    assert(lowered.t == harris(1, 1).toExpr.t)
    val prog = util.printTime("codegen",
      gen.OpenCLKernel(lowered, "harris"))

    val dumbLowering = lowerOCL(ocl.harrisSeqWrite)
    val goldProg = gen.OpenCLKernel(dumbLowering, "harrisGold")

    val random = new scala.util.Random()
    val input = Array.fill(3, Hi, Wi)(random.nextFloat() * 10.0f)

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

  test("harrisBufferedVecUnaligned(3, 4) generates valid OpenCL") {
    assert(Wo % 4 == 0)
    checkOCL(lowerOCL(ocl.harrisBufferedVecUnaligned(3, 4)),
      LocalSize(1), GlobalSize(1))
  }

  test("harrisBufferedVecUnalignedSplitPar(3, 4) generates valid OpenCL") {
    assert(Wo % 4 == 0)
    checkOCL(lowerOCL(ocl.harrisSplitPar(strip, 4,
      ocl.harrisBufferedVecUnaligned(3, 4))),
      LocalSize(1), GlobalSize(Ho / strip))
  }

  test("harrisBufferedVecUnalignedSplitPar(3, 8) generates valid OpenCL") {
    assert(Wo % 8 == 0)
    checkOCL(lowerOCL(ocl.harrisSplitPar(strip, 8,
      ocl.harrisBufferedVecUnaligned(3, 8))),
      LocalSize(1), GlobalSize(Ho / strip))
  }

  test("harrisBufferedVecAligned(3, 4) generates valid OpenCL") {
    assert(Wo % 4 == 0)
    checkOCL(lowerOCL(ocl.harrisBufferedVecAligned(3, 4)),
      LocalSize(1), GlobalSize(1))
  }

  test("harrisBufferedVecAlignedSplitPar(3, 4) generates valid OpenCL") {
    assert(Wo % 4 == 0)
    checkOCL(lowerOCL(ocl.harrisSplitPar(strip, 4,
      ocl.harrisBufferedVecAligned(3, 4))),
      LocalSize(1), GlobalSize(Ho / strip))
  }

  test("harrisBufferedVecAlignedSplitPar(3, 8) generates valid OpenCL") {
    assert(Wo % 8 == 0)
    checkOCL(lowerOCL(ocl.harrisSplitPar(strip, 8,
      ocl.harrisBufferedVecAligned(3, 8))),
      LocalSize(1), GlobalSize(Ho / strip))
  }

  test("harrisBufferedRegRotVecAlignedSplitPar(3, 4) generates valid OpenCL") {
    assert(Wo % 4 == 0)
    checkOCL(lowerOCL(ocl.harrisSplitPar(strip, 4,
      ocl.harrisBufferedRegRotVecAligned(3, 4))),
      LocalSize(1), GlobalSize(Ho / strip))
  }

  test("harrisBufferedRegRotVecAlignedSplitPar(4, 4) generates valid OpenCL") {
    assert(Wo % 4 == 0)
    checkOCL(lowerOCL(ocl.harrisSplitPar(strip, 4,
      ocl.harrisBufferedRegRotVecAligned(4, 4))),
      LocalSize(1), GlobalSize(Ho / strip))
  }

  test("harrisBufferedRegRotVecAlignedSplitPar(3, 8) generates valid OpenCL") {
    assert(Wo % 8 == 0)
    checkOCL(lowerOCL(ocl.harrisSplitPar(strip, 8,
      ocl.harrisBufferedRegRotVecAligned(3, 8))),
      LocalSize(1), GlobalSize(Ho / strip))
  }

  test("harrisBuffered rewrite generates valid OpenCL") {
    val typed = util.printTime("infer", harris(1, 1).toExpr)
    val lowered = rewrite.ocl.harrisBuffered(typed).get
    checkOCL(lowered, LocalSize(1), GlobalSize(1))
  }

  test("harrisBufferedSplitPar rewrite generates valid OpenCL") {
    val typed = util.printTime("infer", harris(strip, 1).toExpr)
    val lowered = rewrite.ocl.harrisBufferedSplitPar(strip)(typed).get
    checkOCL(lowered, LocalSize(1), GlobalSize(Ho / strip))
  }

  test("harrisBufferedVecUnalignedSplitPar rewrite generates valid OpenCL") {
    assert(Wo % 8 == 0)
    val typed = util.printTime("infer", harris(strip, 8).toExpr)

    val lowered4 =
      rewrite.ocl.harrisBufferedVecUnalignedSplitPar(4, strip)(typed).get
    checkOCL(lowered4, LocalSize(1), GlobalSize(Ho / strip))

    val lowered8 =
      rewrite.ocl.harrisBufferedVecUnalignedSplitPar(8, strip)(typed).get
    checkOCL(lowered8, LocalSize(1), GlobalSize(Ho / strip))
  }

  test("harrisBufferedVecAlignedSplitPar rewrite generates valid OpenCL") {
    assert(Wo % 8 == 0)
    val typed = util.printTime("infer", harris(strip, 8).toExpr)

    val lowered4 =
      rewrite.ocl.harrisBufferedVecAlignedSplitPar(4, strip)(typed).get
    checkOCL(lowered4, LocalSize(1), GlobalSize(Ho / strip))

    val lowered8 =
      rewrite.ocl.harrisBufferedVecAlignedSplitPar(8, strip)(typed).get
    checkOCL(lowered8, LocalSize(1), GlobalSize(Ho / strip))
  }

  test("harrisBufferedRegRotVecAlignedSplitPar rewrite generates valid OpenCL") {
    assert(Wo % 8 == 0)
    val typed = util.printTime("infer", harris(strip, 8).toExpr)

    val lowered4 =
      rewrite.ocl.harrisBufferedRegRotVecAlignedSplitPar(4, strip)(typed).get
    checkOCL(lowered4, LocalSize(1), GlobalSize(Ho / strip))

    val lowered8 =
      rewrite.ocl.harrisBufferedRegRotVecAlignedSplitPar(8, strip)(typed).get
    checkOCL(lowered8, LocalSize(1), GlobalSize(Ho / strip))
  }
}
