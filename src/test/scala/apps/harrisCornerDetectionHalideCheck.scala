package apps

import harrisCornerDetectionHalide._
import apps.{harrisCornerDetectionHalideRewrite => rewrite}
import rise.core._

class harrisCornerDetectionHalideCheck extends shine.test_util.Tests {
  test("harris typechecks") {
    val typed = util.printTime("infer", types.infer(harris))
    println(typed.t)
  }

  test("harris is rewritten with circular buffers") {
    val typed = util.printTime("infer", types.infer(harris))
    rewrite.circularBuffers(typed).get
  }
}
