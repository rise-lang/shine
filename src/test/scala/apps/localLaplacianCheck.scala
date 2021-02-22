package apps

import apps.localLaplacian._
import rise.core.DSL._
import rise.core._
import util._

class localLaplacianCheck extends test_util.TestsWithExecutor {
  private val H = 64
  private val W = 80
  private val pyramidLevels = 4 // 8 for bigger image
  private val levels = 8
  private val alpha = 1.0f / (levels - 1).toFloat
  private val beta = 1.0f

  test("localLaplacian typechecks") {
    println(localLaplacian(2).toExpr.t)
  }

  def lowerOMP(e: ToBeTyped[Expr]): Expr =
    harrisCornerDetectionHalideRewrite.unrollDots(util.printTime("infer", e.toExpr)).get

  def checkOMP(lowered: Expr): Unit = {
    val localLaplacian = util.printTime("codegen", gen.openmp.function("localLaplacian").asStringFromExpr(lowered))

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
         | $localLaplacian
         |
         | ${cameraPipelineCheck.read_csv("uint16_t")}
         |
         | int main(int argc, char** argv) {
         |   uint16_t* input = malloc(${3 * H * W} * sizeof(uint16_t));
         |   uint16_t* gold = malloc(${3 * H * W} * sizeof(uint16_t));
         |   uint16_t* output = malloc(${3 * H * W} * sizeof(uint16_t));
         |
         |   read_csv_uint16_t(${3 * H * W}, input, "data/golds/local_laplacian/input.dump");
         |   read_csv_uint16_t(${3 * H * W}, gold, "data/golds/local_laplacian/output.dump");
         |
         |   localLaplacian(output, $levels, $H, $W, $alpha, $beta, input);
         |
         |   int errors = 0;
         |   for (int c = 0; c < 3; c++) {
         |     for (int y = 0; y < $H; y++) {
         |       for (int x = 0; x < $W; x++) {
         |         int i = (c * $H + y) * $W + x;
         |         // TODO: reduce this output difference
         |         uint16_t epsilon = 655; // 0.01 * range_of(uint16_t)
         |         if (abs((int32_t)gold[i] - (int32_t)output[i]) > epsilon) {
         |           if (errors < 20) {
         |             fprintf(stderr, "%.4hu != %.4hu\\n", gold[i], output[i]);
         |           }
         |           errors++;
         |         }
         |       }
         |     }
         |   }
         |
         |   fprintf(stderr, "%d errors\\n", errors);
         |
         |   free(input);
         |   free(gold);
         |   free(output);
         |   return (errors > 0) ? (1) : (0);
         | }
         |""".stripMargin
    util.printTime("execute", util.Execute(testCode))
  }

  test("localLaplacianNaivePar generates OpenMP code") {
    checkOMP(lowerOMP(omp.localLaplacianNaivePar(pyramidLevels)))
  }

  test("remap generates OpenMP code") {
    val lowered = omp.remapNaivePar
    val remap = util.printTime("codegen", gen.openmp.function("remap").asStringFromExpr(lowered))

    val elems = (levels-1)*256*2 + 1
    val testCode =
      s"""
         | #include <stdlib.h>
         | #include <stdio.h>
         | #include <stdint.h>
         | #include <math.h>
         |
         | $remap
         |
         | ${cameraPipelineCheck.read_csv("float")}
         |
         | int main(int argc, char** argv) {
         |   float* gold = malloc($elems * sizeof(float));
         |   float* output = malloc($elems * sizeof(float));
         |
         |   read_csv_float($elems, gold, "data/golds/local_laplacian/remap.dump");
         |
         |   remap(output, $levels, $alpha);
         |
         |   int exit_status = 0;
         |   for (int i = 0; i < $elems; i++) {
         |     if (fabs(gold[i] - output[i]) > 0.001) {
         |       fprintf(stderr, "%.4f != %.4f\\n", gold[i], output[i]);
         |       exit_status = 1;
         |       break;
         |     }
         |   }
         |
         |   free(gold);
         |   free(output);
         |   return exit_status;
         | }
         |""".stripMargin
    util.printTime("execute", util.Execute(testCode))
  }

  test("lookup generates OpenMP code") {
    val lowered = lowerOMP(omp.lookupNaivePar)
    val lookup = util.printTime("codegen", gen.openmp.function("lookup").asStringFromExpr(lowered))

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
         |
         | $lookup
         |
         | ${cameraPipelineCheck.read_csv("uint16_t")}
         | ${cameraPipelineCheck.read_csv("float")}
         |
         | int main(int argc, char** argv) {
         |   uint16_t* input = malloc(${3 * H * W} * sizeof(uint16_t));
         |   float* gold = malloc(${levels * H * W} * sizeof(float));
         |   float* output = malloc(${levels * H * W} * sizeof(float));
         |
         |   read_csv_uint16_t(${3 * H * W}, input, "data/golds/local_laplacian/input.dump");
         |   read_csv_float(${levels * H * W}, gold, "data/golds/local_laplacian/gPyramid_init.dump");
         |
         |   lookup(output, $levels, $H, $W, $alpha, $beta, input);
         |
         |   int exit_status = 0;
         |   for (int l = 0; l < $levels; l++) {
         |     for (int y = 0; y < $H; y++) {
         |       for (int x = 0; x < $W; x++) {
         |         int i = (l * $H + y) * $W + x;
         |         if (fabs(gold[i] - output[i]) > 0.001) {
         |           fprintf(stderr, "%.4f != %.4f\\n", gold[i], output[i]);
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
}
