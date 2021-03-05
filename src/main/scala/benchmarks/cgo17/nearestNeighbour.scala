package benchmarks.cgo17

import apps.nearestNeighbour._
import benchmarks.core._
import util._

object nearestNeighbour {
  def withSize(N: Int, sampleCount: Int): Unit = {
    val random = new scala.util.Random()
    val locations = Array.fill(2 * N)(random.nextFloat())
    val lat = random.nextFloat()
    val lng = random.nextFloat()

    val kernel = gen.opencl.kernel.fromExpr(nn)

    val stats = Seq(
      ("original", benchmark(sampleCount, runOriginalKernel("NearestNeighbour.cl", locations, lat, lng)._2)),
      ("dpia", benchmark(sampleCount, runKernel(kernel, locations, lat, lng)._2)),
    )
    println(s"runtime over $sampleCount runs")
    stats.foreach { case (name, stat) => println(s"$name: $stat") }
  }

  def main(args: Array[String]): Unit = {
    withExecutor {
      withSize(8388608, 10)
      withSize(33554432, 5)
    }
  }
}
