package apps.stencil

import apps.stencil.j2d9pt._

class j2d9ptCheck extends test_util.Tests {
  private val M_small = 4096
  private val N_small = 4096

  private val M_big = 8192
  private val N_big = 8192

  test("j2d9ptHighLevel type checks") {
    println(j2d9ptHighLevel.t)
  }
/*
  test("gaussianKepler generates valid code") {
    println(gen.opencl.kernel.asStringFromExpr(gaussianKepler))
  }
 */
}
