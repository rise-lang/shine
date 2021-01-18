package apps

import rise.core._
import rise.core.DSL._
import unsharpMask._
import util._

class unsharpMaskCheck extends test_util.TestsWithExecutor {
  private val H = 20
  private val W = 80
  private val sigma = 1.5f

  test("unsharp typechecks") {
    println(unsharp(1).toExpr.t)
  }

  def lowerOMP(e: ToBeTyped[Expr]): Expr =
    harrisCornerDetectionHalideRewrite.unrollDots(util.printTime("infer", e.toExpr)).get

  def checkOMP(lowered: Expr): Unit = {
    val unsharp = util.printTime("codegen", gen.openmp.function("unsharp").asStringFromExpr(lowered))

    val testCode =
      s"""
         | #include <stdlib.h>
         | #include <stdio.h>
         | #include <math.h>
         |
         | $unsharp
         |
         | ${cameraPipelineCheck.read_csv("float")}
         |
         | int main(int argc, char** argv) {
         |   float* input = malloc(${3 * H * W} * sizeof(float));
         |   float* gold = malloc(${3 * H * W} * sizeof(float));
         |   float* output = malloc(${3 * H * W} * sizeof(float));
         |
         |   read_csv_float(${3 * H * W}, input, "data/golds/unsharp/input.dump");
         |   read_csv_float(${3 * H * W}, gold, "data/golds/unsharp/output.dump");
         |
         |   unsharp(output, $H, $W, input);
         |
         |   int exit_status = 0;
         |   for (int c = 0; c < 3; c++) {
         |     for (int y = 0; y < $H; y++) {
         |       for (int x = 0; x < $W; x++) {
         |         int i = (c * $H + y) * $W + x;
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

  test("unsharpNaivePar generates OpenMP code") {
    checkOMP(lowerOMP(omp.unsharpNaivePar(l(sigma))))
  }
}
