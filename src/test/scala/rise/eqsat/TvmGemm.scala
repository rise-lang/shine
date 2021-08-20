package rise.eqsat

import rise.core.{Expr => RiseExpr}
import rise.elevate.tvmGemm
import Basic.proveEquiv

class TvmGemm extends test_util.Tests {
  test("TVM GEMM") {
    val mm: RiseExpr = tvmGemm.mm
    val baseline = tvmGemm.baseline(mm).get
    val blocking = tvmGemm.blocking(mm).get
    val vectorization = tvmGemm.vectorization(mm).get
    val loopPerm = tvmGemm.loopPerm(mm).get
    val arrayPacking = tvmGemm.arrayPacking(mm).get
    val cacheBlocks = tvmGemm.cacheBlocks(mm).get
    val parallel = tvmGemm.par(mm).get

    // TODO: splitJoin
    proveEquiv(mm, Seq(
      baseline/*, blocking, vectorization, loopPerm, arrayPacking, cacheBlocks, parallel*/
    ), Seq(
      rules.eta, rules.beta, rules.betaNat,
      rules.reduceSeq, rules.reduceSeqMapFusion
    ))
  }
}
