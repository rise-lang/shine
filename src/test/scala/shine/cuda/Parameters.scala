package shine.cuda

import rise.core.DSL.fun
import rise.core.types.f32
import util.gen

class Parameters extends test_util.Tests {
  test("Output scalar OpenCL") {
    gen.cuda.kernel.fromExpr(fun(f32)(vs => vs))
  }

  test("Output scalar CUDA") {
    gen.cuda.kernel.fromExpr(fun(f32)(vs => vs))
  }
}
