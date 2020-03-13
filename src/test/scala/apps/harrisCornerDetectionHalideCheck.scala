package apps

import harrisCornerDetectionHalide._
import apps.{harrisCornerDetectionHalideRewrite => rewrite}
import rise.core._
import util.gen

class harrisCornerDetectionHalideCheck extends shine.test_util.Tests {
  test("harris typechecks") {
    val typed = util.printTime("infer", types.infer(harris))
    println(typed.t)
  }

  test("harris buffered generates valid code") {
    val typed = util.printTime("infer", types.infer(harrisBuffered))
    val lowered = rewrite.unrollDots(typed)
    util.printTime("codegen", gen.OpenMPProgram(lowered))
  }

  test("splitPar rewrite generates valid code") {
    val typed = util.printTime("infer", types.infer(harris))
    val lowered = rewrite.splitPar(typed)
    util.printTime("codegen", gen.OpenMPProgram(lowered))
  }

  test("circularBuffers rewrite generates valid code") {
    val typed = util.printTime("infer", types.infer(harris))
    val lowered = rewrite.circularBuffers(typed)
    util.printTime("codegen", gen.OpenMPProgram(lowered))
  }
}
