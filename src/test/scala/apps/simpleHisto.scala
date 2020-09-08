package apps

import rise.core.DSL._
import rise.core.Expr
import rise.core.types._
import rise.core.TypeLevelDSL._
import rise.core._
import rise.openCL.DSL._
import util.gen

class simpleHisto extends shine.test_util.TestsWithExecutor {

  private def isT(N: NatIdentifier, K: NatIdentifier) = ArrayType(N, IndexType(K))
  private def histosT(N: NatIdentifier, M: NatIdentifier) = ArrayType(N, ArrayType(M, int))

  val add = fun(x => fun(a => x + a))
  def id: Expr = fun(x => x)

  test("Simple Histogram Test") {

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

    val simpleHistoTest = nFun(n => nFun(numBins => nFun(chunkSize => fun(isT(n, numBins))(is =>
      is |> // n.NatType
      split(chunkSize) |> // n/chunkSize.chunkSize.NatType
      mapGlobal(
        fun(chunk =>
          reduceByIndexSeq(AddressSpace.Global)(add)(
            generate(fun(IndexType(numBins))(_ => l(0))) |>
              mapSeq(id)
          )(chunk)(
            generate(fun(IndexType(chunkSize))(_ => l(1)))
          )
        ) >> mapSeq(id)
      ) |> // n/chunkSize.numBins.int
      toGlobal |>
      // reduce all subhistograms
      reduceHistos
    ))))

    gen.OpenCLKernel(simpleHistoTest)
  }
}
