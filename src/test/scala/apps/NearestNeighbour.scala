package apps

import nearestNeighbour._
import util.gen

class NearestNeighbour extends shine.test_util.TestsWithExecutor {
  private val N = 512

  test("nearest neighbour versions produce same results") {
    val random = new scala.util.Random()
    val locations = Array.fill(2 * N)(random.nextFloat)
    val lat = random.nextFloat
    val lng = random.nextFloat

    shine.test_util.runsWithSameResult(Seq(
      ("original", runOriginalKernel("NearestNeighbour.cl", locations, lat, lng)),
      ("dpia", runKernel(gen.OpenCLKernel(nn), locations, lat, lng))
    ))
  }
}
