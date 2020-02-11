package shine.DPIA.Primitives

import rise.core.DSL._
import rise.core.types._
import rise.OpenCL.DSL._
import util.gen

class Let extends shine.test_util.Tests {
  val id = fun(x => x)

  // TODO: it feels like toMem and let are closely related, should be merged?
  test("let private value") {
    def plusNum(n: Int, code: String): Unit = {
      "\\+".r.findAllIn(code).length shouldBe n
    }
    plusNum(2, gen.OpenCLKernel(fun(int)(x =>
      toPrivate(x + l(2)) |> fun(y => y * y)
    )).code)
    plusNum(2, gen.OpenCLKernel(fun(int)(x =>
      (x + l(2)) |> let(fun(y => y * y))
    )).code)
    plusNum(1, gen.OpenCLKernel(fun(int)(x =>
      toPrivate(x + l(2)) |> let(fun(y => y * y))
    )).code)
  }
}
