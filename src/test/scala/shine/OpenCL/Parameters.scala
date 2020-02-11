package shine.OpenCL

import rise.core.DSL._
import rise.core.types._
import util.gen

class Parameters extends shine.test_util.Tests {
  val m = 4 // vector width

  test("Output scalar") {
    gen.OpenCLKernel(fun(f32)(vs => vs))
  }

  test("Output vector") {
    gen.OpenCLKernel(fun(VectorType(m, f32))(vs => vs))
  }

  test("Output array") {
    gen.OpenCLKernel(
      nFun(n => fun(ArrayType(n, f32))(vs => vs |> mapSeq(fun(x => x))))
    )
  }
}
