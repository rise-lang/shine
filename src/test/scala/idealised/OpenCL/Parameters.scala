package idealised.OpenCL

import lift.core.DSL._
import lift.core.types._
import idealised.util.gen

class Parameters extends idealised.util.Tests {
  val m = 4 // vector width

  test("Output scalar") {
    gen.OpenCLKernel(fun(float)(vs => vs))
  }

  test("Output vector") {
    gen.OpenCLKernel(fun(VectorType(m, float))(vs => vs))
  }

  test("Output array") {
    gen.OpenCLKernel(nFun(n => fun(ArrayType(n, float))(vs => vs)))
  }
}