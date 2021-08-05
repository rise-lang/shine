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
    ProveEquiv.init()
      .withFilter(ArrayDimensionPredicate(4) && ASTSizePredicate(80))
      .runCNF(mm, variants, Seq(
      rules.eta, rules.betaExtract, rules.betaNatExtract,
      // rules.combinatory.compositionAssoc1,
      rules.combinatory.compositionAssoc2,
      rules.combinatory.compositionIntro,
      rules.combinatory.compositionLeftId,
      rules.combinatory.compositionRightId,
      rules.combinatory.mapFusion,
      rules.combinatory.mapFusion2,
      rules.combinatory.mapFission, // TODO: mapFission2, etc?
      rules.combinatory.transposePairAfter,
      rules.combinatory.mapMapFBeforeTranspose,
      rules.combinatory.mapMapFBeforeTranspose1,
      rules.reduceSeq,
      rules.combinatory.reduceSeqMapFusion,
      rules.combinatory.reduceSeqMapFusion2,
      rules.combinatory.splitJoin(32),
      // rules.combinatory.blockedReduce(4),
    ))
  }

  test("tiling") {
    val mm: Expr = tvmGemm.mm
    val start = tvmGemm.baseline(mm).get
    val goal = tvmGemm.blockingPartial(mm).get

    ProveEquiv.init()
      .withFilter(ArrayDimensionPredicate(4) && ASTSizePredicate(80))
      .withEndRules(Seq(
        rules.combinatory.compositionAssoc2,
        rules.combinatory.mapFusion,
        rules.combinatory.mapFusion2,
      ))
      .runCNF(start, goal, Seq(
        rules.eta, rules.betaExtract, rules.betaNatExtract,
        rules.combinatory.compositionAssoc2,
        rules.combinatory.compositionIntro,
        rules.combinatory.compositionLeftId,
        rules.combinatory.compositionRightId,
        rules.combinatory.mapFusion,
        rules.combinatory.mapFusion2,
        rules.combinatory.mapFission,
        rules.combinatory.transposePairAfter,
        rules.combinatory.mapMapFBeforeTranspose,
        rules.combinatory.mapMapFBeforeTranspose1,
        rules.combinatory.splitJoin(32),
    ))
  }

  test("blocking partial") {
    val mm: Expr = tvmGemm.mm
    val start = tvmGemm.blockingPartial(mm).get
    val goals = Seq(
      tvmGemm.blockingPartial2(mm).get,
      tvmGemm.blockingPartial3(mm).get
    )

    ProveEquiv.init()
      .runBENF(start, goals, Seq(
        rules.eta, rules.betaExtract, rules.betaNatExtract,
        rules.reduceSeqMapFission,
        rules.undoReduceSeqForAdd,
        rules.blockedReduce(4),
      ))

    ProveEquiv.init()
      .runCNF(start, goals, Seq(
        rules.eta, rules.betaExtract, rules.betaNatExtract,
        rules.combinatory.compositionAssoc2,
        rules.combinatory.compositionIntro,
        rules.combinatory.compositionElim,
        rules.combinatory.compositionLeftId,
        rules.combinatory.compositionRightId,
        rules.undoReduceSeqForAdd,
        rules.combinatory.reduceSeqMapFission,
        rules.combinatory.blockedReduce(4),
      ))
  }

  test("blocking reorder") {
    val mm: Expr = tvmGemm.mm
    val start = tvmGemm.blockingPartial3(mm).get
    val goal = tvmGemm.blocking(mm).get
/*
    ProveEquiv.init()
      .runBENF(start, goal, Seq(
        rules.eta, rules.betaExtract, rules.betaNatExtract,
        rules.mapFission,
        rules.reduceSeqMapFusion,
        rules.splitBeforeMap,
        rules.liftReduceSeq,
        rules.liftReduceSeq2,
      ))

 */
    ProveEquiv.init()
      .runCNF(start, goal, Seq(
        rules.eta, rules.betaExtract, rules.betaNatExtract,
        rules.combinatory.compositionAssoc1,
        rules.combinatory.compositionAssoc2,
        rules.combinatory.compositionIntro,
        rules.combinatory.compositionElim,
        rules.combinatory.compositionLeftId,
        rules.combinatory.compositionRightId,
        // rules.combinatory.mapFusion,
        rules.combinatory.mapFission,
        rules.combinatory.splitBeforeMap,
        rules.combinatory.splitBeforeMap2,
        rules.combinatory.reduceSeqMapFusion,
        rules.combinatory.liftReduceSeq,
        rules.combinatory.liftReduceSeq2,
      ))
  }

  ignore("vectorize") {
    val mm: Expr = tvmGemm.mm
    val start = tvmGemm.blocking(mm).get
    val goal = tvmGemm.vectorization(mm).get

    ProveEquiv.init().runCNF(start, goal, Seq(
      rules.eta, rules.betaExtract, rules.betaNatExtract,
      rules.combinatory.compositionAssoc2,
      rules.combinatory.compositionIntro,
      rules.combinatory.compositionElim,
      rules.combinatory.compositionLeftId,
      rules.combinatory.compositionRightId,
      rules.combinatory.vectorize.after(32),
      // TODO:
      // rules.combinatory.vectorize.beforeMap
    ))
  }

  test("lift-reduce-seq") {
    import rise.core.DSL._
    import rise.core.types._
    import rise.core.primitives._
    import rise.core.DSL.Type._

    val t = (16`.`32`.`64`.`(f32 x f32)) ->: (16`.`f32)

    val start: Expr = map(reduceSeq(fun(acc => fun(y =>
      acc + reduceSeq(fun(acc => fun(y =>
        acc + (fst(y) * snd(y))
      )))(lf32(0))(y)
    )))(lf32(0))) :: t

    val goal: Expr = fun(in => reduceSeq(fun(acc => fun(y =>
      map(fun(x => fst(x) + reduceSeq(fun(acc => fun(y =>
        acc + (fst(y) * snd(y))
      )))(lf32(0))(snd(x))))(zip(acc)(y))
    )))(generate(fun(_ => lf32(0))))(transpose(in))) :: t

    ProveEquiv.init().runBENF(start, goal, Seq(
      rules.eta, rules.betaExtract, rules.betaNatExtract,
      rules.liftReduceSeq
    ))

    ProveEquiv.init().runCNF(start, goal, Seq(
      rules.eta, rules.betaExtract, rules.betaNatExtract,
      rules.combinatory.compositionIntro,
      rules.combinatory.compositionAssoc2,
      rules.combinatory.liftReduceSeq
    ))
  }

  test("lift-reduce-seq-2") {
    import rise.core.DSL._
    import rise.core.types._
    import rise.core.primitives._
    import rise.core.DSL.Type._

    val t = (16`.`(f32 x (32`.`(f32 x f32)))) ->: (16`.`f32)

    val start: Expr = map(fun(x =>
      fst(x) + reduceSeq(fun(acc => fun(y =>
        acc + (fst(y) * snd(y))
      )))(lf32(0))(snd(x))
    )) :: t

    val goal: Expr = fun(in => reduceSeq(fun(acc => fun(y =>
      map(fun(e8673 => fst(e8673) + (fst(snd(e8673)) * snd(snd(e8673)))))(
        zip(acc)(y))
    )))(fst(unzip(in)))(transpose(snd(unzip(in))))) :: t

    ProveEquiv.init().runBENF(start, goal, Seq(
      rules.eta, rules.betaExtract, rules.betaNatExtract,
      rules.liftReduceSeq2
    ))

    ProveEquiv.init().runCNF(start, goal, Seq(
      rules.eta, rules.betaExtract, rules.betaNatExtract,
      rules.combinatory.compositionIntro,
      rules.combinatory.compositionElim,
      rules.combinatory.compositionAssoc1,
      rules.combinatory.compositionAssoc2,
      rules.combinatory.liftReduceSeq2
    ))
  }
}
