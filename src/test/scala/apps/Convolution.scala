package apps

import apps.convolution._
import rise.core.DSL._
import util.gen

class Convolution extends test_util.TestsWithExecutor {
  private val inputSize_small = 4096

  test("high-level convolutions typecheck") {
    logger.debug(blurXHighLevel.t)
    logger.debug(blurYHighLevel.t)
  }

  test("convolution versions produce same results") {
    val N = inputSize_small // TODO: this is still big for a test
    val random = new scala.util.Random()
    val matrix = Array.fill(N, N)(random.nextFloat() * 10.0f)
    val weights = Array.fill(17)(random.nextFloat())

    val (lsX, gsX) = blurXTiled2DSizes(N)
    val (lsY, gsY) = blurYTiled2DTiledLoadingTransposedSizes(N)

    runsWithSameResult(Seq(
      ("originalX (CG017)", runOriginalKernel("CGO17_ConvolutionColumn_small.cl",
        N, lsX, gsX, matrix, weights)),
      ("dpiaX", runKernel(gen.opencl.kernel(lsX, gsX).fromExpr(blurXTiled2D(N)),
        lsX, gsX, matrix, weights))
    ))
    runsWithSameResult(Seq(
      ("originalY (CG017)", runOriginalKernel("CGO17_ConvolutionRow_small.cl",
        N, lsY, gsY, matrix, weights)),
      ("dpiaY", runKernel(
        gen.opencl.kernel(lsY, gsY).fromExpr(blurYTiled2DTiledLoadingTransposed(N)),
        lsY, gsY, matrix, weights))
    ))
  }
}
