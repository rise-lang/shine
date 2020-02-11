package shine.DPIA.Semantics

import rise.core.DSL._
import rise.core.types._

import util.gen

class SplitSlide extends shine.test_util.Tests {

  val n = 8
  val sz = 3
  val sp = 1

  test("slide-split") {
    val e = fun(ArrayType(130, f32))(xs =>
      xs |> slide(sz)(sp) |> split(n) |> mapSeq(mapSeq(mapSeq(fun(x => x))))
    )

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 3
  }

  test("slide-map(slide)") {
    val e = fun(ArrayType(130, f32))(xs =>
      xs |> slide(n + sz - sp)(n) |> map(slide(sz)(sp)) |> mapSeq(
        mapSeq(mapSeq(fun(x => x)))
      )
    )

    val code = gen.CProgram(e).code

    "for".r.findAllIn(code).length shouldBe 3
  }
}
