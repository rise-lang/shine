package benchmarks.cgo17

import apps.molecularDynamics._
import benchmarks.core._
import util._

object molecularDynamics {
  def main(args: Array[String]): Unit = {
    val N = 1024
    val M = 128

    val random = new scala.util.Random()
    val particles = Array.fill(N * 4)(random.nextFloat() * 20.0f)
    val particlesTuple = particles.sliding(4, 4).map { case Array(a, b, c, d) => (a, b, c, d) }.toArray
    val neighbours = buildNeighbourList(particlesTuple, M).transpose

    val kernel = gen.OpenCLKernel(shoc)

    withExecutor {
      val sampleCount = 10
      val stats = Seq(
        ("original", benchmark(sampleCount, runOriginalKernel("MolecularDynamics.cl", particles, neighbours)._2)),
        ("dpia", benchmark(sampleCount, runKernel(kernel, particles, neighbours)._2))
      )
      println(s"runtime over $sampleCount runs")
      stats.foreach { case (name, stat) => println(s"$name: $stat") }
    }
  }
}
