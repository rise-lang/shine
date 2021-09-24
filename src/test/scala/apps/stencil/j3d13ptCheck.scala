package apps.stencil

import apps.stencil.j3d13pt._

class j3d13ptCheck extends test_util.Tests {
  private val M_small = 256
  private val N_small = 256
  private val O_small = 256

  private val M_big = 512
  private val N_big = 512
  private val O_big = 512

  test("j3d13ptHighLevel type checks") {
    println(j3d13ptHighLevel.t)
  }
/*
  test("gaussianKepler generates valid code") {
    println(gen.opencl.kernel.asStringFromExpr(gaussianKepler))
  }
 */
}
