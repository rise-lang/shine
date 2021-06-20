package rise.eqsat

import rise.core.Expr
import rise.elevate.tvmGemm
import ProveEquiv.syntax._
import rise.eqsat.PredicateDSL._

/*
object TvmGemm {
  def main(args: Array[String]): Unit = {
    val mm: Expr = tvmGemm.mm
    val variants = util.printTime("Elevate rewrite", Seq(
      tvmGemm.baseline(mm).get,
      tvmGemm.blockingPartial(mm).get,
      // tvmGemm.vectorization(mm).get,
      // tvmGemm.loopPerm(mm).get,
      // tvmGemm.arrayPacking(mm).get,
      // tvmGemm.cacheBlocks(mm).get,
      // tvmGemm.par(mm).get
    ))

    ProveEquiv.init()
      .withFilter(ArrayDimensionPredicate(5) && ASTSizePredicate(100))
      .runCNF(mm, variants, Seq(
      // rules.eta, rules.betaExtract, rules.betaNatExtract,
      // rules.combinatory.compositionAssoc1,
      rules.combinatory.compositionAssoc2,
      // rules.combinatory.compositionIntro,
      // rules.combinatory.compositionLeftId,
      // rules.combinatory.compositionRightId,
      // rules.combinatory.mapFusion,
      rules.combinatory.mapFission,
      // rules.combinatory.transposePairAfter,
      // rules.combinatory.mapMapFBeforeTranspose,
      rules.combinatory.transposeAroundMapMapF,
      rules.reduceSeq,
      rules.combinatory.reduceSeqMapFusion,
      rules.combinatory.splitJoin(32),
      // rules.combinatory.blockedReduce(4),
    ))
  }
}
*/

class TvmGemm extends test_util.Tests {
  private val proveEquiv = ProveEquiv.init()

  test("TVM GEMM") {
    val mm: Expr = tvmGemm.mm
    val variants = util.printTime("Elevate rewrite", Seq(
      tvmGemm.baseline(mm).get,
      tvmGemm.blockingPartial(mm).get,
      // tvmGemm.vectorization(mm).get,
      // tvmGemm.loopPerm(mm).get,
      // tvmGemm.arrayPacking(mm).get,
      // tvmGemm.cacheBlocks(mm).get,
      // tvmGemm.par(mm).get
    ))
/*
    proveEquivBENF(mm, variants(0), Seq(
      rules.eta,
      rules.betaExtract, rules.betaNatExtract,
      // rules.beta, rules.betaNat,
      // rules.mapFusion, rules.mapFission,
      rules.reduceSeq, rules.reduceSeqMapFusion,
      // rules.splitJoin(32), rules.blockedReduce(4),
      // rules.splitBeforeMap, rules.transposePairAfter,
      // rules.mapMapFBeforeTranspose,
      // rules.liftReduce
    ))
*/
    proveEquiv.runCNF(mm, variants, Seq(
      rules.eta, rules.betaExtract, rules.betaNatExtract,
      rules.combinatory.compositionAssoc1,
      rules.combinatory.compositionAssoc2,
      rules.combinatory.compositionIntro,
      rules.combinatory.compositionLeftId,
      rules.combinatory.compositionRightId,
      rules.combinatory.mapFusion,
      rules.combinatory.mapFission,
      rules.combinatory.transposePairAfter,
      rules.combinatory.mapMapFBeforeTranspose,
      rules.reduceSeq,
      rules.combinatory.reduceSeqMapFusion,
      rules.combinatory.splitJoin(32),
      // rules.combinatory.blockedReduce(4),
    ))
  }
}
