package apps.stencil

import apps.stencil.heat3d._

class heat3dCheck extends test_util.Tests {
  private val M_small = 256
  private val N_small = 256
  private val O_small = 256

  private val M_big = 512
  private val N_big = 512
  private val O_big = 512

  test("heat3dHighLevel type checks") {
    println(heat3dHighLevel.t)
  }
/*
  test("gaussianKepler generates valid code") {
    println(gen.opencl.kernel.asStringFromExpr(gaussianKepler))
  }
 */
}
