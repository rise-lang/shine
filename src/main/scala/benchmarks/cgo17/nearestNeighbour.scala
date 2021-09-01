package benchmarks.cgo17

import apps.nearestNeighbour._
import benchmarks.core._
import util._

object nearestNeighbour {
  def withSize(N: Int, sampleCount: Int): Seq[(String, TimeStat[Time.ms])] = {
    val random = new scala.util.Random()
    val locations = Array.fill(2 * N)(random.nextFloat())
    val lat = random.nextFloat()
    val lng = random.nextFloat()

    val kernel = gen.opencl.kernel.fromExpr(nnOcl)

    val stats = Seq(
      ("original", benchmark(sampleCount,
        runOriginalKernel("CGO17_NN.cl", locations, lat, lng)._2)),
      ("dpia", benchmark(sampleCount, runKernel(kernel, locations, lat, lng)._2)),
    )
    println(s"runtime over $sampleCount runs")
    stats.foreach { case (name, stat) => println(s"$name: $stat") }
    stats
  }

  def bench(): Seq[(String, Seq[(String, TimeStat[Time.ms])])] = Seq(
    ("small", withSize(8388608, 10)),
    ("large", withSize(33554432, 10))
  )

  def main(args: Array[String]): Unit = {
    withExecutor {
      withSize(8388608, 10)
      withSize(33554432, 5)
    }
  }
}
