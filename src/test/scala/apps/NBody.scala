package apps

import nbody._
import util.gen
import shine.OpenCL._

class NBody extends test_util.TestsWithExecutor {
  private val N = 512

  test("high level nbody typechecks") {
    println(nbodyHighLevel.t)
  }

  test("nbody versions produce same results") {
    val random = new scala.util.Random()
    val pos = Array.fill(N * 4)(random.nextFloat() * random.nextInt(10))
    val vel = Array.fill(N * 4)(random.nextFloat() * random.nextInt(10))

    val localSizeAMD = LocalSize(128)
    val globalSizeAMD = GlobalSize(N)

    val localSizeNVIDIA = LocalSize((tileX, tileY))
    val globalSizeNVIDIA = GlobalSize((N, tileY))

    test_util.runsWithSameResult(Seq(
      ("original AMD", runOriginalKernel("NBody-AMD.cl", localSizeAMD, globalSizeAMD, pos, vel)),
      ("original NVIDIA", runOriginalKernel("NBody-NVIDIA.cl", localSizeNVIDIA, globalSizeNVIDIA, pos, vel)),
      ("dpia AMD", runKernel(gen.opencl.kernel.fromExpr(nbodyAMD), localSizeAMD, globalSizeAMD, pos, vel)),
      ("dpia NVIDIA", runKernel(gen.opencl.kernel.fromExpr(nbodyNVIDIA), localSizeNVIDIA, globalSizeNVIDIA, pos, vel))
    ))
  }

  // FIXME: generated code calls update too many times
  // related to pair assignment in the TranslationContext
  test("nbody AMD version calls update only once") {
    val code = gen.opencl.kernel.asStringFromExpr(nbodyAMD)
    "update\\(".r.findAllIn(code).length shouldBe 2
  }
}
