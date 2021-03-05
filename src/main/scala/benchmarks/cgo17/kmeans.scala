package benchmarks.cgo17

import apps.kmeans._
import benchmarks.core._
import shine.OpenCL._
import util._

object kmeans {
  def withSize(P: Int, C: Int, F: Int, sampleCount: Int): Unit = {
    val random = new scala.util.Random()
    val features = Array.fill(F, P)(random.nextFloat())
    val clusters = Array.fill(C, F)(random.nextFloat())

    val kernel = gen.opencl.kernel.fromExpr(apps.kmeans.kmeansLL)

    val stats = Seq(
      ("original", benchmark(sampleCount, runOriginalKernel("CGO17_KMeans.cl", features, clusters)._2)),
      ("dpia", benchmark(sampleCount, runKernel(kernel, features, clusters)._2)),
    )
    println(s"runtime over $sampleCount runs")
    stats.foreach { case (name, stat) => println(s"$name: $stat") }
  }

  def main(args: Array[String]): Unit = {
    val P1 = 204800
    val P2 = 819200
    val C = 5
    val F = 34
    withExecutor {
      withSize(P1, C, F, 6)
      withSize(P2, C, F, 3)
    }
  }
}
