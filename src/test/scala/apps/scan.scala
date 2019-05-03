package apps

import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import idealised.util.gen

class scan extends idealised.util.Tests {
  private val simpleScan = fun(ArrayType(8, float))(array =>
    array |> scanSeq(fun(x => fun(a => a + x)))(l(0.0f))
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
