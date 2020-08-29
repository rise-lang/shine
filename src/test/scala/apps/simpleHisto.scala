package apps

import rise.core.DSL._
import rise.core.Expr
import rise.core.types._
import rise.core._
import rise.openCL.DSL._
//import shine.OpenCL.get_global_size
import util.gen

class simpleHisto extends shine.test_util.TestsWithExecutor {

  private def xsT(N: NatIdentifier) = ArrayType(N, int)

  val add = fun(x => fun(a => x + a))
  def id: Expr = fun(x => x)

  test("Simple Histogram Test") {
    val k: Nat = 4

    val simpleHistoTest = nFun(n => fun(xsT(n))(is =>
      is |>
      split(1) |>
      mapGlobal(
        oclReduceSeq(AddressSpace.Private)(
          fun(a => fun(i =>
            a |>
            mapSeq(fun(x => x))
          ))
        )(
          generate(fun(IndexType(k))(_ => l(0))) |>
          mapSeq(fun(x => x))
        )
      )
    ))

    gen.OpenCLKernel(simpleHistoTest)
  }
}
