package apps

import kmeans._
import util.gen

class KMeans extends test_util.TestsWithExecutor {
  private val P = 1024
  private val C = 5
  private val F = 34

  test("kmeans versions produce same results") {
    val random = new scala.util.Random()
    val features = Array.fill(F, P)(random.nextFloat())
    val clusters = Array.fill(C, F)(random.nextFloat())

    test_util.runsWithSameResult(Seq(
      ("original", runOriginalKernel("KMeans.cl", features, clusters)),
      ("dpia", runKernel(gen.opencl.kernel.fromExpr(apps.kmeans.kmeans), features, clusters))
    ))
  }
}
