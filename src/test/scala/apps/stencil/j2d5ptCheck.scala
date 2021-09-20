package apps.stencil

import apps.stencil.j2d5pt._

class j2d5ptCheck extends test_util.Tests {
  private val M_small = 4096
  private val N_small = 4096

  private val M_big = 8192
  private val N_big = 8192

  test("j2d5ptHighLevel type checks") {
    println(j2d5ptHighLevel.t)
  }
/*
  test("gaussianKepler generates valid code") {
    println(gen.opencl.kernel.asStringFromExpr(gaussianKepler))
  }
 */
}
