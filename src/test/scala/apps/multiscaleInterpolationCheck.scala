package apps

import apps.multiscaleInterpolation._
import rise.core.DSL._
import rise.core._
import util._

class multiscaleInterpolationCheck extends test_util.TestsWithExecutor {
  private val levels = 4
  private val H = 32
  private val W = 64

  test("interpolate typechecks") {
    println(interpolate(levels, 1).toExpr.t)
  }

  def lowerOMP(e: ToBeTyped[Expr]): Expr =
    harrisCornerDetectionHalideRewrite.unrollDots(util.printTime("infer", e.toExpr)).get

  def checkOMP(lowered: Expr): Unit = {
    val dumbLowering = lowerOMP(omp.interpolateNaivePar(levels))
    val goldProg = gen.OpenMPProgram(dumbLowering, "interpolateGold")

    val prog = util.printTime("codegen",
      gen.OpenMPProgram(lowered, "interpolate"))

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
         |   float* input = malloc(${4 * H * W} * sizeof(float));
         |   float* gold = malloc(${4 * H * W} * sizeof(float));
         |   float* output = malloc(${4 * H * W} * sizeof(float));
         |
         |   for (int i = 0; i < ${4 * H * W}; i++) {
         |     input[i] = (float)((i + 179) % 256) / 25.6f;
         |   }
         |
         |   ${goldProg.function.name}(gold, $H, $W, input);
         |   ${prog.function.name}(output, $H, $W, input);
         |
         |   int exit_status = 0;
         |   for (int y = 0; y < $H; y++) {
         |     for (int x = 0; x < $W; x++) {
         |       int i = y * $W + x;
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

  test("interpolateNaivePar generates OpenMP code") {
    // FIXME: checks against itself
    checkOMP(lowerOMP(omp.interpolateNaivePar(levels)))
  }
}