package benchmarks.cgo17

import apps.gemv._
import benchmarks.core._
import shine.DPIA
import shine.OpenCL._
import util._

object gemv {
  def withSize(N: Int, M: Int, sampleCount: Int): Unit = {
    val rand = new scala.util.Random
    val mat = Array.fill(M, N)(rand.nextFloat() * 5)
    val matT = mat.transpose
    val xs = Array.fill(N)(rand.nextFloat() * 5)
    val ys = Array.fill(M)(rand.nextFloat() * 5)
    val alpha = rand.nextFloat() * 5
    val beta = rand.nextFloat() * 5

    val kernelN = gen.opencl.kernel(
      Some(cgo17_phraseDepLocalAndGlobalSize),
      "KERNEL"
    ).fromExpr(ocl.blastN)
    val kernelT = gen.opencl.kernel(
      Some(cgo17_phraseDepLocalAndGlobalSize),
      "KERNEL"
    ).fromExpr(ocl.blastT)

    val stats = Seq(
      ("original N", benchmark(sampleCount, runOriginal("CGO17_GEMV_N.cl",
        mat, xs, ys, alpha, beta)._2)),
      ("dpia N", benchmark(sampleCount, runKernel(kernelN,
        mat, xs, ys, alpha, beta)._2)),
      ("original T", benchmark(sampleCount, runOriginal("CGO17_GEMV_T.cl",
        matT, xs, ys, alpha, beta)._2)),
      ("dpia T", benchmark(sampleCount, runKernel(kernelT,
        matT, xs, ys, alpha, beta)._2))
    )
    println(s"runtime over $sampleCount runs for size ${(N, M)}")
    stats.foreach { case (name, stat) => println(s"$name: $stat") }
  }

  def main(args: Array[String]): Unit = withExecutor {
    withSize(4096, 4096, 20)
    withSize(8192, 8192, 10)
  }
}
