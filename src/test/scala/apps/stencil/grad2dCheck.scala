package apps.stencil

import apps.stencil.grad2d._

class grad2dCheck extends test_util.Tests {
  private val M_small = 4096
  private val N_small = 4096

  private val M_big = 8192
  private val N_big = 8192

  test("gaussianHighLevel type checks") {
    println(grad2dHighLevel.t)
  }
/*
  test("gaussianKepler generates valid code") {
    println(gen.opencl.kernel.asStringFromExpr(gaussianKepler))
  }
 */
}
