package apps

import rise.core.DSL._
import rise.core.primitives._
import rise.core.types._
import util.gen

class scan extends test_util.Tests {
  private val simpleScan = fun(ArrayType(8, f32))(array =>
    array |> scanSeq(add)(l(0.0f))
  )

  test("Simple scan compiles to syntactically correct C") {
    gen.CProgram(simpleScan)
  }

  test("Simple scan compiles to syntactically correct OpenMP") {
    gen.OpenMPProgram(simpleScan)
  }

  // currently fails do to a missing address space at a new
  ignore("Simple scan compiles to syntactically correct OpenCL") {
    // TODO
  }

}
