package benchmarks.cgo17

import apps.convolution._
import rise.core.DSL._
import benchmarks.core._
import util._

object convolution {
  def withSize(N: Int, sampleCount: Int, originalSuffix: String): Unit = {
    val random = new scala.util.Random()
    val matrix = Array.fill(N, N)(random.nextFloat * 10.0f)
    val weights = Array.fill(17)(random.nextFloat)

    val (lsX, gsX) = blurXTiled2DSizes(N)
    val (lsY, gsY) = blurYTiled2DTiledLoadingTransposedSizes(N)
    val kernelX = gen.OpenCLKernel(blurXTiled2D(N))
    val kernelY = gen.OpenCLKernel(blurYTiled2DTiledLoadingTransposed(N))

    val stats = Seq(
      ("original X", benchmark(sampleCount, runOriginalKernel(s"CGO17_ConvolutionColumn_$originalSuffix.cl",
        N, lsX, gsX, matrix, weights)._2)),
      ("original Y", benchmark(sampleCount, runOriginalKernel(s"CGO17_ConvolutionRow_$originalSuffix.cl",
        N, lsY, gsY, matrix, weights)._2)),
      ("dpia X", benchmark(sampleCount, runKernel(kernelX, lsX, gsX, matrix, weights)._2)),
      ("dpia Y", benchmark(sampleCount, runKernel(kernelY, lsY, gsY, matrix, weights)._2))
    )
    println(s"runtime over $sampleCount runs for $originalSuffix")
    stats.foreach { case (name, stat) => println(s"$name: $stat") }
  }

  def main(args: Array[String]): Unit = {
    val inputSize_small = 4096

    withExecutor {
      withSize(inputSize_small, 5, "small")
      // TODO: big
    }
  }
}
