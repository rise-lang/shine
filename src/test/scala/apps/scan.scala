package apps

import rise.core.DSL._
import rise.core.primitives._
import rise.core.types.DataType._
import util.gen
import util.gen.c.function

class scan extends test_util.Tests {
  private val simpleScan = fun(ArrayType(8, f32))(array =>
    array |> scanSeq(add)(lf32(0.0f))
  )

  test("Simple scan compiles to syntactically correct C") {
    function.asStringFromExpr(simpleScan)
  }

  test("Simple scan compiles to syntactically correct OpenMP") {
    gen.openmp.function.asStringFromExpr(simpleScan)
  }

  // currently fails do to a missing address space at a new
  ignore("Simple scan compiles to syntactically correct OpenCL") {
    // TODO
  }

}
