package benchmarks.cgo17

import apps.molecularDynamics._
import benchmarks.core._
import util._

object molecularDynamics {
  private val Ns = Seq(12288, 24576, 36864, 73728)
  private val M = 128 // Max number of nearest neighbors

  def withSize(N: Int, sampleCount: Int): Unit = {
    val random = new scala.util.Random()
    val particles = Array.fill(N * 4)(random.nextFloat() * 20.0f)
    val particlesTuple = particles.sliding(4, 4)
      .map { case Array(a, b, c, d) => (a, b, c, d) }.toArray
    val neighbours = buildNeighbourList(particlesTuple, M).transpose

    val kernel = gen.opencl.kernel.fromExpr(shocLL)

    val stats = Seq(
      ("original", benchmark(sampleCount,
        runOriginalKernel("CGO17_MolecularDynamics.cl", particles, neighbours)._2)),
      ("dpia", benchmark(sampleCount, runKernel(kernel, particles, neighbours)._2)),
    )
    println(s"runtime over $sampleCount runs")
    stats.foreach { case (name, stat) => println(s"$name: $stat") }
  }

  def main(args: Array[String]): Unit = {
    withExecutor {
      withSize(Ns(0), 10)
      // FIXME: this takes forever because of scala-side input generation
      // withSize(Ns(3), 5)
    }
  }
}
