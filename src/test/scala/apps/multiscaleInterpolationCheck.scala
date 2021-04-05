package apps

import apps.multiscaleInterpolation._
import rise.core.DSL._
import rise.core._
import util._

class multiscaleInterpolationCheck extends test_util.TestsWithExecutor {
  private val levels = 10
  private val H = 64
  private val W = 80

  test("interpolate typechecks") {
    logger.debug(interpolate(levels, 1).t)
  }

  def lowerOMP(e: ToBeTyped[Expr]): Expr =
    harrisCornerDetectionHalideRewrite.unrollDots(util.printTime("infer", e.toExpr)).get

  def checkOMP(lowered: Expr): Unit = {
    val interpolate = util.printTime("codegen", gen.openmp.function("interpolate").asStringFromExpr(lowered))

    val (ci, hi, wi) = (4, H, W)
    val (co, ho, wo) = (3, H, W)
    val testCode =
      s"""
         | #include <stdlib.h>
         | #include <stdio.h>
         | #include <math.h>
         |
         | $interpolate
         |
         | ${cameraPipelineCheck.read_csv("float")}
         |
         | int main(int argc, char** argv) {
         |   float* input = malloc(${ci * hi * wi} * sizeof(float));
         |   float* gold = malloc(${co * ho * wo} * sizeof(float));
         |   float* output = malloc(${co * ho * wo} * sizeof(float));
         |
         |   read_csv_float(${ci * hi * wi}, input, "data/golds/interpolate/input.dump");
         |   read_csv_float(${co * ho * wo}, gold, "data/golds/interpolate/output.dump");
         |
         |   interpolate(output, $hi, $wi, input);
         |
         |   int exit_status = 0;
         |   for (int c = 0; c < $co; c++) {
         |     for (int y = 0; y < $ho; y++) {
         |       for (int x = 0; x < $wo; x++) {
         |         int i = (c * $ho + y) * $wo + x;
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

  test("interpolateNaivePar generates OpenMP code") {
    checkOMP(lowerOMP(omp.interpolateNaivePar(levels)))
  }
}