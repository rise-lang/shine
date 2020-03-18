package apps

import harrisCornerDetectionHalide._
import apps.{harrisCornerDetectionHalideRewrite => rewrite}
import rise.core._
import util.gen

class harrisCornerDetectionHalideCheck extends shine.test_util.Tests {
  test("harris typechecks") {
    val typed = util.printTime("infer", types.infer(harris))
    println(typed.t)
  }

  val H = 128
  val W = 64 // x vector width
  val Hi = H + 4
  val Wi = W + 2 // x vector width

  def checkC(lowered: Expr): Unit = {
    val dumbLowering = rewrite.unrollDots(types.infer(harrisSeqWrite))
    val goldProg = gen.CProgram(dumbLowering, "harrisGold")

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
         |     if (fabs(gold[i] - output[i]) > 0.01) {
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

  test("harrisBuffered generates valid code") {
    val typed = util.printTime("infer", types.infer(harrisBuffered))
    val lowered = rewrite.unrollDots(typed)
    checkC(lowered)
  }

  test("harrisBufferedVecUnaligned generates valid code") {
    val typed = util.printTime("infer", types.infer(harrisBufferedVecUnaligned))
    val lowered = rewrite.unrollDots(typed)
    checkC(lowered)
  }

  test("harrisBufferedVecUnalignedSplitPar generates valid code") {
    val typed = util.printTime("infer", types.infer(
      harrisBufferedVecUnalignedSplitPar))
    val lowered = rewrite.unrollDots(typed)
    checkC(lowered)
  }

  test("harrisBufferedVecAligned generates valid code") {
    val typed = util.printTime("infer", types.infer(harrisBufferedVecAligned))
    val _ = rewrite.unrollDots(typed)
    // check(lowered) FIXME, shuffle in OpenMP?
  }

  test("splitPar rewrite generates valid code") {
    val typed = util.printTime("infer", types.infer(harris))
    val lowered = rewrite.splitPar(typed)
    util.printTime("codegen", gen.OpenMPProgram(lowered))
  }

  test("circularBuffers rewrite generates valid code") {
    val typed = util.printTime("infer", types.infer(harris))
    val lowered = rewrite.circularBuffers(typed)
    util.printTime("codegen", gen.OpenMPProgram(lowered))
  }
}
