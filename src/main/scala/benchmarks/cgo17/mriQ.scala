package benchmarks.cgo17

import apps.mriQ._
import benchmarks.core._
import util.gen

object mriQ {
  def main(args: Array[String]): Unit = {
    val K = 64
    val X = 64

    val random = new scala.util.Random()
    val phiR = Array.fill(K)(random.nextFloat)
    val phiI = Array.fill(K)(random.nextFloat)

    val kernelPhiMag = gen.OpenCLKernel(computePhiMag)

    val x = Array.fill(X)(random.nextFloat)
    val y = Array.fill(X)(random.nextFloat)
    val z = Array.fill(X)(random.nextFloat)
    val Qr = Array.fill(X)(random.nextFloat)
    val Qi = Array.fill(X)(random.nextFloat)
    val kvalues = Array.fill(4 * K)(random.nextFloat)

    val kernelQ = gen.OpenCLKernel(computeQ)

    withExecutor {
      val sampleCount = 10
      val stats = Seq(
        ("original phiMag", benchmark(sampleCount, runOriginalComputePhiMag("CGO17_ComputePhiMag.cl", phiR, phiI)._2)),
        ("dpia phiMag", benchmark(sampleCount, runComputePhiMag(kernelPhiMag, phiR, phiI)._2)),
        ("original Q", benchmark(sampleCount, runOriginalComputeQ("CGO17_ComputeQ.cl", x, y, z, Qr, Qi, kvalues)._2)),
        ("dpia Q", benchmark(sampleCount, runComputeQ(kernelQ, x, y, z, Qr, Qi, kvalues)._2))
      )
      println(s"runtime over $sampleCount runs")
      stats.foreach { case (name, stat) => println(s"$name: $stat") }
    }
  }
}
