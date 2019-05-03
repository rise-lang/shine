package apps

import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import idealised.util.gen

class stencil extends idealised.util.Tests {

  private val simpleStencil = nFun(n => fun(ArrayType(n, float))(xs =>
    xs |> slide(3)(1) |> mapSeq(fun(nbh =>
      nbh |> reduceSeq(fun(x => fun(a => x + a)))(l(0.0f))
    ))
  ))

  test("Simple stencil compiles to syntactically correct C") {
    gen.CProgram(simpleStencil)
  }

  test("Simple scan compiles to syntactically correct OpenMP") {
    gen.OpenMPProgram(simpleStencil)
  }

  // currently fails do to a missing address space at a new
  ignore ("Simple scan compiles to syntactically correct OpenCL") {
    // TODO
  }
}
