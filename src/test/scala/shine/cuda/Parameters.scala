package shine.cuda

import rise.core.DSL.fun
import rise.core.types.f32
import util.gen

class Parameters extends test_util.Tests {
  test("Output scalar OpenCL") {
    gen.OpenCLKernel(fun(f32)(vs => vs))
  }

  test("Output scalar CUDA") {
    gen.cuKernel(fun(f32)(vs => vs))
  }
}