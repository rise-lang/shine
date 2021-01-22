package apps

import mriQ._
import util.gen

class MRIQ extends test_util.TestsWithExecutor {
  private val K = 32
  private val X = 32

  test("computePhiMag versions produce same results") {
    val random = new scala.util.Random()
    val phiR = Array.fill(K)(random.nextFloat())
    val phiI = Array.fill(K)(random.nextFloat())

    test_util.runsWithSameResult(Seq(
      ("original", runOriginalComputePhiMag("CGO17_ComputePhiMag.cl", phiR, phiI)),
      ("dpia", runComputePhiMag(gen.opencl.kernel.fromExpr(computePhiMag), phiR, phiI))
    ))
  }

  test("computeQ versions produce same results") {
    val random = new scala.util.Random()
    val x = Array.fill(X)(random.nextFloat())
    val y = Array.fill(X)(random.nextFloat())
    val z = Array.fill(X)(random.nextFloat())
    val Qr = Array.fill(X)(random.nextFloat())
    val Qi = Array.fill(X)(random.nextFloat())
    val kvalues = Array.fill(4 * K)(random.nextFloat())

    test_util.runsWithSameResult(Seq(
      ("original", runOriginalComputeQ("CGO17_ComputeQ.cl", x, y, z, Qr, Qi, kvalues)),
      ("dpia", runComputeQ(gen.opencl.kernel.fromExpr(computeQ), x, y, z, Qr, Qi, kvalues))
    ))
  }
}
