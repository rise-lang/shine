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
    val numBins: Nat = 4

    def incrementBin(
      numBins: Int,
      input: Expr,
      hist: Identifier,
      binCounter: Int = 0
    ): Expr = {
      // If (primitive):
      // exp[t, read] -> exp[bool, read] ->
      //  (exp[t, read] -> exp[s, write]) ->
      //  (exp[t, read] -> exp[s, write]) -> exp[s, write]

      //If(input, (input == l(binCounter)),
      // Set(hist, idx(binCounter, numBins), (idx(a, lidx(binCounter, n)))),
      // incrementBin(numBins, input, hist, ++binCounter))
    }

    val simpleHistoTest = nFun(n => nFun(chunkSize => fun(xsT(n))(is =>
      is |> // n.int
      split(chunkSize) |> //n/chunkSize.chunkSize.int
      mapGlobal(
        oclReduceSeq(AddressSpace.Private)(
          fun(a => // numBins.int
            fun(i => // int
              // if (i == 0) set(a, lidx(0, numBins), a[lidx(0, numBins)]+1)) else
              //   if (i == 1) set(a, lidx(1, numBins), a[lidx(1, numBins)]+1)) else
              //     if (i == 2) set(a, lidx(2, numBins), a[lidx(2, numBins)]+1))
          ))
        )(
          generate(fun(IndexType(numBins))(_ => l(0))) |> // numBins.int, read
          mapSeq(fun(x => x)) // numBins.int, write
        )
      )
    )))

    gen.OpenCLKernel(simpleHistoTest)
  }
}
