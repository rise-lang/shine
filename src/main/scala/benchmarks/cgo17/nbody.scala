package benchmarks.cgo17

import apps.nbody._
import benchmarks.core._
import shine.OpenCL._
import util._

object nbody {
  def withSize(N: Int, sampleCount: Int): Unit = {
    val random = new scala.util.Random()
    val pos = Array.fill(N * 4)(random.nextFloat() * random.nextInt(10))
    val vel = Array.fill(N * 4)(random.nextFloat() * random.nextInt(10))

    val localSizeAMD = LocalSize(128)
    val globalSizeAMD = GlobalSize(N)
    val localSizeNVIDIA = LocalSize((tileX, tileY))
    val globalSizeNVIDIA = GlobalSize((N, tileY))
    val kernelAMD = gen.opencl.kernel.fromExpr(nbodyAMD)
    val kernelNVIDIA = gen.opencl.kernel.fromExpr(nbodyNVIDIA)

    val stats = Seq(
      ("original AMD", benchmark(sampleCount, runOriginalKernel("NBody-AMD.cl", localSizeAMD, globalSizeAMD, pos, vel)._2)),
      ("dpia AMD", benchmark(sampleCount, runKernel(kernelAMD, localSizeAMD, globalSizeAMD, pos, vel)._2)),
      ("original NVIDIA", benchmark(sampleCount, runOriginalKernel("NBody-NVIDIA.cl", localSizeNVIDIA, globalSizeNVIDIA, pos, vel)._2)),
      ("dpia NVIDIA", benchmark(sampleCount, runKernel(kernelNVIDIA, localSizeNVIDIA, globalSizeNVIDIA, pos, vel)._2))
    )
    println(s"runtime over $sampleCount runs")
    stats.foreach { case (name, stat) => println(s"$name: $stat") }
  }

  def main(args: Array[String]): Unit = {
    withExecutor {
      withSize(16384, 6)
      withSize(131072, 3)
    }
  }
}
