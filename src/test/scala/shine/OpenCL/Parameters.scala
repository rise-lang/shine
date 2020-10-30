package shine.OpenCL

import rise.core.TypedDSL._
import rise.core.primitives._
import rise.core.types._
import util.gen

class Parameters extends test_util.Tests {
  val m = 4 // vector width

  test("Output scalar") {
    gen.OpenCLKernel(fun(f32)(vs => vs))
  }

  test("Output vector") {
    gen.OpenCLKernel(fun(VectorType(m, f32))(vs => vs))
  }

  test("Output array") {
    gen.OpenCLKernel(
      depFun((n: Nat) => fun(ArrayType(n, f32))(vs => vs |> mapSeq(fun(x => x))))
    )
  }
}
