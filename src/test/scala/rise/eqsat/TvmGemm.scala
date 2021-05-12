package rise.eqsat

import rise.core.Expr
import rise.elevate.tvmGemm
import Basic.proveEquivBENF

class TvmGemm extends test_util.Tests {
  test("TVM GEMM") {
    val mm: Expr = tvmGemm.mm
    val variants = util.printTime("Elevate rewrite", Seq(
      tvmGemm.baseline(mm).get,
      tvmGemm.blocking(mm).get,
      // tvmGemm.vectorization(mm).get,
      // tvmGemm.loopPerm(mm).get,
      // tvmGemm.arrayPacking(mm).get,
      // tvmGemm.cacheBlocks(mm).get,
      // tvmGemm.par(mm).get
    ))

    proveEquivBENF(mm, variants, Seq(
      rules.eta, rules.beta, rules.betaNat,
      rules.mapFusion, rules.mapFission,
      rules.reduceSeq, rules.reduceSeqMapFusion,
      rules.splitJoin(32), rules.blockedReduce(4),
      rules.splitBeforeMap, rules.transposePairAfter,
      rules.mapMapFBeforeTranspose,
      // rules.liftReduce
    ))
  }
}
