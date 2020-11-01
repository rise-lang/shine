package shine.OpenCL

import rise.core.dsl._
import rise.core.exprs.primitives._
import rise.core.types._
import rise.core.util.gen
import rise.opencl.dsl._

class Vectors extends test_util.Tests {
  val vs = 4
  val id = fun(x => x)

  test("generate OpenCL code for vectorized load & store") {
    gen.OpenCLKernel(
      depFun((n: Nat) => fun(ArrayType(n * vs, f32))(a =>
        a |> asVectorAligned(vs) |> mapGlobal(id) |> asScalar
      ))
    )
  }

  // FIXME: generates invalid vstore
  test("generate OpenCL code for unaligned vector load & store") {
    gen.OpenCLKernel(
      depFun((n: Nat) => fun(ArrayType(n * vs, f32))(a =>
        a |> drop(1) |> asVector(vs) |> mapGlobal(id) |> asScalar
      ))
    )
  }
}
