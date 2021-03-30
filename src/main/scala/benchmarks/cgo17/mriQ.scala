package benchmarks.cgo17

import apps.mriQ._
import benchmarks.core._
import util._

object mriQ {
  def withSize(K: Int, X: Int, sampleCount: Int): Unit = {
    val random = new scala.util.Random()
    val phiR = Array.fill(K)(random.nextFloat())
    val phiI = Array.fill(K)(random.nextFloat())

    val x = Array.fill(X)(random.nextFloat())
    val y = Array.fill(X)(random.nextFloat())
    val z = Array.fill(X)(random.nextFloat())
    val Qr = Array.fill(X)(random.nextFloat())
    val Qi = Array.fill(X)(random.nextFloat())
    val kvalues = Array.fill(4 * K)(random.nextFloat())

    val phiMagKernel = gen.opencl.kernel.fromExpr(computePhiMagOcl)
    val qKernel = gen.opencl.kernel.fromExpr(computeQOcl)

    val stats = Seq(
      ("original PhiMag", benchmark(sampleCount,
        runOriginalComputePhiMag("CGO17_ComputePhiMag.cl", phiR, phiI)._2)),
      ("dpia PhiMag", benchmark(sampleCount, runComputePhiMag(phiMagKernel, phiR, phiI)._2)),
      ("original Q", benchmark(sampleCount,
        runOriginalComputeQ("CGO17_ComputeQ.cl", x, y, z, Qr, Qi, kvalues)._2)),
      ("dpia Q", benchmark(sampleCount, runComputeQ(qKernel, x, y, z, Qr, Qi, kvalues)._2)),
    )
    println(s"runtime over $sampleCount runs")
    stats.foreach { case (name, stat) => println(s"$name: $stat") }
  }

  def main(args: Array[String]): Unit = {
    withExecutor {
      withSize(3072, 32768, 8)
      withSize(2048, 262144, 4)
    }
  }
}
