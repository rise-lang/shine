package benchmarks.cgo17

import apps.kmeans._
import benchmarks.core._
import util._

object kmeans {
  def main(args: Array[String]): Unit = {
    val P = 1024
    val C = 5
    val F = 34

    val random = new scala.util.Random()
    val features = Array.fill(F, P)(random.nextFloat)
    val clusters = Array.fill(C, F)(random.nextFloat)

    val kernel = gen.OpenCLKernel(apps.kmeans.kmeans)

    withExecutor {
      val sampleCount = 10
      val stats = Seq(
        ("original", benchmark(sampleCount, runOriginalKernel(s"KMeans.cl", features, clusters)._2)),
        ("dpia", benchmark(sampleCount, runKernel(kernel, features, clusters)._2))
      )
      println(s"runtime over $sampleCount runs")
      stats.foreach { case (name, stat) => println(s"$name: $stat") }
    }
  }
}
