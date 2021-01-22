package shine.OpenCL

import rise.core.DSL._
import rise.core.primitives._
import rise.core.types._
import util.gen

class Parameters extends test_util.Tests {
  val m = 4 // vector width

  test("Output scalar") {
    gen.opencl.kernel.fromExpr(fun(f32)(vs => vs))
  }

  test("Output vector") {
    gen.opencl.kernel.fromExpr(fun(VectorType(m, f32))(vs => vs))
  }

  test("Output array") {
    gen.opencl.kernel.fromExpr(
      depFun((n: Nat) => fun(ArrayType(n, f32))(vs => vs |> mapSeq(fun(x => x))))
    )
  }
}
