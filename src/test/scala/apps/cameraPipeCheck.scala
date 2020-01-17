package apps

import cameraPipe._
import util._
import rise.core._
import rise.core.types._
import rise.core.DSL._
import rise.core.TypeLevelDSL._

class cameraPipeCheck extends test_util.TestsWithExecutor {
  test("cameraPipe passes type inference") {
    println("avg: " + infer(avg).t)
    println(
      "interpolate: " + infer(
        nFun(h =>
          nFun(w =>
            fun(h `.` w `.` 2 `.` i16)(a =>
              interpolate(Image(0, w, 0, h, a)).expr
            )
          )
        )
      ).t
    )
    println(
      "point_abs_diff: " + infer(
        nFun(h =>
          nFun(w =>
            fun(h `.` w `.` 2 `.` i16)(a =>
              point_abs_diff(Image(0, w, 0, h, a)).expr
            )
          )
        )
      ).t
    )
    println("demosaic: " + printTime(infer(demosaic)).t)
    println(
      "hot pixel suppression: " + printTime(infer(hot_pixel_suppression)).t
    )
    println("deinterleave: " + printTime(infer(deinterleave)).t)
    println("color correction: " + printTime(infer(color_correct)).t)
    println("apply curve: " + printTime(infer(apply_curve)).t)
    println("sharpen: " + printTime(infer(sharpen)).t)
  }
}
