package apps

import rise.core.DSL._
import rise.core.Expr
import rise.core.types._
import rise.core.TypeLevelDSL._
import rise.core._
import rise.openCL.DSL._
import util.gen

class simpleHisto extends shine.test_util.TestsWithExecutor {

  // scalastyle:off

  private def isT(N: NatIdentifier) = ArrayType(N, NatType)
  private def histosT(N: NatIdentifier, M: NatIdentifier) = ArrayType(N, ArrayType(M, int))

  val add = fun(x => fun(a => x + a))
  def id: Expr = fun(x => x)

  test("Simple Histogram Test") {
    // val numBins: Nat = 2000

    /*def incrementBin(
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
    }*/

    val reduceHistos = implN(n => implN(numBins => fun(histosT(n, numBins))(histos =>
      histos |> // n.numBins.int
        oclReduceSeq(AddressSpace.Global)(
          fun(acc_histo => // numBins.int
            fun(cur_histo => // numBins.int
              zip(acc_histo)(cur_histo) |> // 2.numBins.int
                mapGlobal(fun(x => fst(x) + snd(x)))
            )
          )
        )(
          generate(fun(IndexType(numBins))(_ => l(0))) |> // numBins.int
            mapSeq(id) //numBins.int
        ) |>
        mapSeq(id)
    )))

    val simpleHistoTest = nFun(n => nFun(numBins => nFun(chunkSize => fun(isT(n))(is =>
      is |> // n.NatType
      split(chunkSize) |> // n/chunkSize.chunkSize.NatType
      mapGlobal(
        oclReduceSeq(AddressSpace.Global)(
          fun(histo => // numBins.int
            fun(i => // nat
              histo |>
              mapSeq(fun(histo => histo + l(1))) // int
              // if (i == 0) set(a, lidx(0, numBins), a[lidx(0, numBins)]+1)) else
              //   if (i == 1) set(a, lidx(1, numBins), a[lidx(1, numBins)]+1)) else
              //     if (i == 2) set(a, lidx(2, numBins), a[lidx(2, numBins)]+1))
            )
          )
        )(
          generate(fun(IndexType(numBins))(_ => l(0))) |> // numBins.int
          mapSeq(id) // numBins.int
        ) >> mapSeq(id)
      ) |> // n/chunkSize.numBins.int
      toGlobal |>
      // reduce all subhistograms
      reduceHistos
    ))))

    gen.OpenCLKernel(simpleHistoTest)
  }

  /* Deprecated, will be removed later
  test("Simplest Histogramm Test") {
    val numBins: Nat = 4

    val simplestHistoTest = nFun(n => fun(isT(n))(is =>
      is |>
      mapGlobal(
        fun(i =>
          generate(fun(IndexType(numBins))(_ => l(0))) |>
          mapSeq(fun(x => x + i)) // test function, to replace with reduce by index operation
        )
      )
    ))

    gen.OpenCLKernel(simplestHistoTest)
  }

  test("Old Histogramm Test") {
    val numBins: Nat = 4

    val oldHistoTest = nFun(n => nFun(chunkSize => fun(isT(n))(is =>
      is |>
        split(chunkSize) |>
        mapGlobal(
          fun(chunk =>
            generate(fun(IndexType(numBins))(_ => l(0))) |>
              mapSeq(fun(x => x)) |>
              toPrivate |>
              fun(histo =>
                chunk |>
                mapSeq(fun(element =>
                  // test function, to replace with reduce by index operation
                  histo |>
                  mapSeq(fun(bin => bin + element))
              )
          )
        )
      ) >> toGlobal >> mapSeq(mapSeq(fun(x => x)))
    ))))

    gen.OpenCLKernel(oldHistoTest)
  } */
}
