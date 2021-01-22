package apps

import nearestNeighbour._
import util.gen

class NearestNeighbour extends test_util.TestsWithExecutor {
  private val N = 512

  test("nearest neighbour versions produce same results") {
    val random = new scala.util.Random()
    val locations = Array.fill(2 * N)(random.nextFloat())
    val lat = random.nextFloat()
    val lng = random.nextFloat()

    test_util.runsWithSameResult(Seq(
      ("original", runOriginalKernel("NearestNeighbour.cl", locations, lat, lng)),
      ("dpia", runKernel(gen.opencl.kernel.fromExpr(nn), locations, lat, lng))
    ))
  }
}
