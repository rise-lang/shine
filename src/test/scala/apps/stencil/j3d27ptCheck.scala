package apps.stencil

import apps.stencil.j3d27pt._

class j3d27ptCheck extends test_util.Tests {
  private val M_small = 256
  private val N_small = 256
  private val O_small = 256

  private val M_big = 512
  private val N_big = 512
  private val O_big = 512

  test("j3d27ptHighLevel type checks") {
    println(j3d27ptHighLevel.t)
  }
/*
  test("gaussianKepler generates valid code") {
    println(gen.opencl.kernel.asStringFromExpr(gaussianKepler))
  }
 */
}
