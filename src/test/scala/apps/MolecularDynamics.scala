package apps

import molecularDynamics._
import util.{Time, TimeSpan, gen}
import shine.OpenCL._

class MolecularDynamics extends test_util.TestsWithExecutor {
  private val N = 1024
  private val M = 128

  test("molecular dynamics versions produce same results") {
    val random = new scala.util.Random()
    val particles = Array.fill(N * 4)(random.nextFloat() * 20.0f)
    val particlesTuple = particles.sliding(4, 4).map { case Array(a, b, c, d) => (a, b, c, d) }.toArray
    val neighbours = buildNeighbourList(particlesTuple, M).transpose

    test_util.runsWithSameResult(Seq(
      ("original", runOriginalKernel("MolecularDynamics.cl", particles, neighbours)),
      ("dpia", runKernel(gen.OpenCLKernel(shoc), particles, neighbours))
    ))
  }
}
