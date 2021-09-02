package benchmarks.cgo17

import apps.kmeans._
import benchmarks.core._
import shine.OpenCL._
import util._

object kmeans {
  def withSize(P: Int, C: Int, F: Int, sampleCount: Int): Seq[(String, TimeStat[Time.ms])] = {
    val random = new scala.util.Random()
    val features = Array.fill(F, P)(random.nextFloat())
    val clusters = Array.fill(C, F)(random.nextFloat())

    val kernel = gen.opencl.kernel(Some(kmeansOclKnownSizes), "KERNEL").fromExpr(apps.kmeans.kmeansOcl)

    val stats = Seq(
      ("original", benchmark(sampleCount, runOriginalKernel("CGO17_KMeans.cl", features, clusters)._2)),
      ("dpia", benchmark(sampleCount, runKernel(kernel, features, clusters)._2)),
    )
    println(s"runtime over $sampleCount runs")
    stats.foreach { case (name, stat) => println(s"$name: $stat") }
    stats
  }

  val P1 = 204800
  val P2 = 819200
  val C = 5
  val F = 34

  def bench(): Seq[(String, Seq[(String, TimeStat[Time.ms])])] = Seq(
    ("small", withSize(P1, C, F, 10)),
    ("large", withSize(P2, C, F, 10))
  )

  def main(args: Array[String]): Unit = {
    withExecutor {
      withSize(P1, C, F, 6)
      withSize(P2, C, F, 3)
    }
  }
}
