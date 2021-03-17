package benchmarks.cgo17

import apps.convolution._
import benchmarks.core._
import util._

object convolutionHosted {
  def withSize(N: Int, sampleCount: Int): Unit = {
    val hostedX = util.gen.opencl.hosted("fun").fromExpr(hosted.blurXTiled2D(N))
    val hostedY = util.gen.opencl.hosted("fun").fromExpr(hosted.blurYTiled2DTiledLoadingTransposed(N))

    val init =
      s"""
         |if (sample == 0) srand(time(NULL));
         |Context ctx = createDefaultContext();
         |Buffer matrix = createBuffer(ctx, $N * $N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
         |Buffer weights = createBuffer(ctx, 17 * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_READ);
         |Buffer output = createBuffer(ctx, $N * $N * sizeof(float), HOST_READ | HOST_WRITE | DEVICE_WRITE);
         |
         |float* m = hostBufferSync(ctx, matrix, $N * $N * sizeof(float), HOST_WRITE);
         |for (int i = 0; i < $N * $N; i++) {
         |  m[i] = (float)(rand())/(float)(RAND_MAX) * 10.0f;
         |}
         |
         |float* w = hostBufferSync(ctx, weights, 17 * sizeof(float), HOST_WRITE);
         |for (int i = 0; i < 17; i++) {
         |  w[i] = (float)(rand())/(float)(RAND_MAX);
         |}
         |""".stripMargin

    val compute =
      s"""
         |fun(ctx, output, matrix, weights);
         |waitFinished(ctx);
         |""".stripMargin

    val finish =
      s"""
         |// TODO: could check output here
         |
         |destroyBuffer(ctx, matrix);
         |destroyBuffer(ctx, weights);
         |destroyBuffer(ctx, output);
         |destroyContext(ctx);
         |""".stripMargin

    val stats = Seq(
      ("dpia X", hostedBenchmark(sampleCount, "one_copy", init, compute, finish, hostedX)),
      ("dpia Y", hostedBenchmark(sampleCount, "one_copy", init, compute, finish, hostedY)),
    )
    println(s"runtime over $sampleCount runs for N=$N")
    stats.foreach { case (name, stat) => println(s"$name: $stat") }
  }

  def main(args: Array[String]): Unit = {
    val inputSize_small = 4096
    val inputSize_large = 8192

    withSize(inputSize_small, 6)
    withSize(inputSize_large, 3)
  }
}

object convolution {
  def withSize(N: Int, sampleCount: Int, originalSuffix: String): Unit = {
    val random = new scala.util.Random()
    val matrix = Array.fill(N, N)(random.nextFloat() * 10.0f)
    val weights = Array.fill(17)(random.nextFloat())

    val (lsX, gsX) = blurXTiled2DSizes(N)
    val (lsY, gsY) = blurYTiled2DTiledLoadingTransposedSizes(N)
    val kernelX = gen.opencl.kernel(lsX, gsX).fromExpr(blurXTiled2D(N))
    val kernelY = gen.opencl.kernel(lsY, gsY).fromExpr(blurYTiled2DTiledLoadingTransposed(N))

    val stats = Seq(
      ("original X", benchmark(sampleCount, runOriginalKernel(s"CGO17_ConvolutionColumn_$originalSuffix.cl",
        N, lsX, gsX, matrix, weights)._2)),
      ("dpia X", benchmark(sampleCount, runKernel(kernelX, lsX, gsX, matrix, weights)._2)),
      ("original Y", benchmark(sampleCount, runOriginalKernel(s"CGO17_ConvolutionRow_$originalSuffix.cl",
        N, lsY, gsY, matrix, weights)._2)),
      ("dpia Y", benchmark(sampleCount, runKernel(kernelY, lsY, gsY, matrix, weights)._2))
    )
    println(s"runtime over $sampleCount runs for $originalSuffix")
    stats.foreach { case (name, stat) => println(s"$name: $stat") }
  }

  def main(args: Array[String]): Unit = {
    val inputSize_small = 4096
    val inputSize_large = 8192

    withExecutor {
      withSize(inputSize_small, 6, "small")
      withSize(inputSize_large, 3, "large")
    }
  }
}
