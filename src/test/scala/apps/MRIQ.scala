package apps

import mriQ._
import util.gen

class MRIQ extends test_util.TestsWithExecutor {
  private val K = 256
  private val X = 512

  test("high level expression typechecks") {
    println(computePhiMagHighLevel.t)
    println(computeQHighLevel.t)
  }

  test("computePhiMag versions produce same results") {
    val random = new scala.util.Random()
    val phiR = Array.fill(K)(random.nextFloat())
    val phiI = Array.fill(K)(random.nextFloat())

    test_util.runsWithSameResult(Seq(
      ("original", runOriginalComputePhiMag("CGO17_ComputePhiMag.cl", phiR, phiI)),
      ("dpia", runComputePhiMag(gen.opencl.kernel.fromExpr(computePhiMagOcl), phiR, phiI))
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
      ("dpia", runComputeQ(gen.opencl.kernel.fromExpr(computeQOcl), x, y, z, Qr, Qi, kvalues))
    ))
  }
}
