package shine.OpenCL

import util.gen
import rise.core.DSL._
import rise.core.types._
import rise.openCL.DSL._

class SetVal extends shine.test_util.Tests {

  private def xsT(N: NatIdentifier) = ArrayType(N, int)

  test("Idx Test") {

    val idxTest = nFun(n => nFun(i => fun(xsT(n))(xs =>
      xs |>
        mapSeq(
          fun(x => x + idx(lidx(i, n), xs))
        )
    )))

    gen.OpenCLKernel(idxTest)
  }

  test("Set Test") {

    val setTest = nFun(n => nFun(i => fun(xsT(n))(xs =>
      generate(fun(IndexType(n))(_ => l(0))) |>
        mapSeq(fun(x => x)) |>
        toGlobal |>
      setVal(lidx(i, n), l(5)) |>
        mapSeq(fun(x => x))
    )))

    gen.OpenCLKernel(setTest)
  }

}
