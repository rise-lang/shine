package apps

import mm._
import shine.OpenCL._
import util._

class mmCheck extends test_util.TestsWithExecutor {
  private val N = 64
  private val M = 128
  private val O = 128

  private def randGold(): (Array[Array[Float]], Array[Array[Float]], Array[Float]) = {
    val rand = new scala.util.Random
    val At = Array.fill(O, N)(rand.nextFloat() * 10)
    val B = Array.fill(O, M)(rand.nextFloat() * 10)
    val gold = computeGold(N, M, O, At, B).flatten
    (At, B, gold)
  }

  test("high-level matrix multiplication typechecks") {
    logger.debug(mmHighLevel.t)
  }

  test("sequential matrix multiplication produces expected result") {
    val (at, b, gold) = randGold()
    val (output, time) = runKernel(gen.opencl.kernel.fromExpr(mmSequential),
      LocalSize(1), GlobalSize(1), at, b)
    util.assertSame(output, gold, "output is different from gold")
    logger.debug(s"time: $time")
  }

  test("amd matrix multiplication produces expected result") {
    val (at, b, gold) = randGold()
    val runs = Seq(
      "original" -> runOriginal("CGO17_MMAMD.cl",
        LocalSize((32, 8)), GlobalSize((M/4, N/8)), at, b),
      "dpia" -> runKernel(gen.opencl.kernel.fromExpr(mmAMD),
        LocalSize((32, 8)), GlobalSize((M/4, N/8)), at, b)
    )
    runs.foreach(r => {
      util.assertSame(r._2._1, gold, s"${r._1} is different from gold")
      logger.debug(s"${r._1} time: ${r._2._2}")
    })
  }

  // CL_OUT_OF_RESOURCES on the CI
  ignore("nvidia matrix multplication produces expected result") {
    val (at, b, gold) = randGold()
    val runs = Seq(
      "original" -> runOriginal("CGO17_MMNVIDIA.cl",
        LocalSize((32, 8)), GlobalSize((M/4, N/8)), at, b),
      "dpia" -> runKernel(gen.opencl.kernel.fromExpr(mmNVIDIA),
        LocalSize((32, 8)), GlobalSize((M/4, N/8)), at, b)
    )
    runs.foreach(r => {
      util.assertSame(r._2._1, gold, s"${r._1} is different from gold")
      logger.debug(s"${r._1} time: ${r._2._2}")
    })
  }
}
