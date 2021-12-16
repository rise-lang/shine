package apps.stencil

import apps.stencil.srad1._
import util.gen

class srad1Check extends test_util.Tests {
  test("srad1HighLevel type checks") {
    println(srad1HighLevel.t)
  }

  test("srad1Nvidia generates valid code") {
    println(gen.opencl.kernel.asStringFromExpr(srad1Nvidia))
  }
}
