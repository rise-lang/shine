package apps

import apps.localLaplacian._
import rise.core.DSL._
import rise.core._
import util._

class localLaplacianCheck extends test_util.TestsWithExecutor {
  private val H = 20
  private val W = 80
  private val pyramidLevels = 8
  private val levels = 8
  private val alpha = 1.0f
  private val beta = 1.0f

  test("unsharp typechecks") {
    println(localLaplacian(2).toExpr.t)
  }

  def lowerOMP(e: ToBeTyped[Expr]): Expr =
    harrisCornerDetectionHalideRewrite.unrollDots(util.printTime("infer", e.toExpr)).get

  def checkOMP(lowered: Expr): Unit = {
    val dumbLowering = lowerOMP(omp.localLaplacianNaivePar(pyramidLevels))
    val goldProg = gen.OpenMPProgram(dumbLowering, "localLaplacianGold")

    val prog = util.printTime("codegen",
      gen.OpenMPProgram(lowered, "localLaplacian"))

    val testCode =
      s"""
         | #include <stdlib.h>
         | #include <stdio.h>
         | #include <stdint.h>
         | #include <math.h>
         |
         | int min_int(int a, int b) {
         |   return (a < b) ? a : b;
         | }
         | int max_int(int a, int b) {
         |   return (a > b) ? a : b;
         | }
         | int clamp_int(int v, int l, int h) {
         |   return min_int(max_int(v, l), h);
         | }
         | float min_f32(float a, float b) {
         |   return (a < b) ? a : b;
         | }
         | float max_f32(float a, float b) {
         |   return (a > b) ? a : b;
         | }
         | float clamp_f32(float v, float l, float h) {
         |   return min_f32(max_f32(v, l), h);
         | }
         |
         | ${goldProg.code}
         |
         | ${prog.code}
         |
         | int main(int argc, char** argv) {
         |   uint16_t* input = malloc(${3 * H * W} * sizeof(uint16_t));
         |   uint16_t* gold = malloc(${3 * H * W} * sizeof(uint16_t));
         |   uint16_t* output = malloc(${3 * H * W} * sizeof(uint16_t));
         |
         |   for (int i = 0; i < ${3 * H * W}; i++) {
         |     input[i] = (i + 179);// % 65536;
         |   }
         |
         |   ${goldProg.function.name}(gold, $levels, $H, $W, $alpha, $beta, input);
         |   ${prog.function.name}(output, $levels, $H, $W, $alpha, $beta, input);
         |
         |   int exit_status = 0;
         |   for (int c = 0; c < 3; c++) {
         |     for (int y = 0; y < $H; y++) {
         |       for (int x = 0; x < $W; x++) {
         |         int i = (c * $H + y) * $W + x;
         |         if (gold[i] != output[i]) {
         |           fprintf(stderr, "%.4hu != %.4hu\\n", gold[i], output[i]);
         |           exit_status = 1;
         |           break;
         |         }
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

  test("localLaplacianNaivePar generates OpenMP code") {
    // FIXME: checks against itself
    checkOMP(lowerOMP(omp.localLaplacianNaivePar(pyramidLevels)))
  }
}
