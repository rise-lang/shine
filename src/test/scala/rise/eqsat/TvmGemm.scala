package rise.eqsat

import apps.tvmGemm
import rise.core.Expr
import ProveEquiv.syntax._
import PredicateDSL._

class TvmGemm extends test_util.Tests {
  test("baseline") {
    val mm: Expr = tvmGemm.mm
    val goal = tvmGemm.baseline(mm).get

    ProveEquiv.init().runBENF(mm, goal, Seq(
      rules.reduceSeq, rules.reduceSeqMapFusion
    ))
  }

  test("blocking partial 1") {
    val mm: Expr = tvmGemm.mm
    val start = tvmGemm.baseline(mm).get
    val goal = tvmGemm.blockingPartial1(mm).get

    ProveEquiv.init()
      .withFilter(ArrayDimensionPredicate(5) && ASTSizePredicate(200))
      .runBENF(start, goal, Seq(
        rules.mapFission,
        // rules.transposeAroundMapMapF,
        rules.transposeAroundMapMapF1M,
        // rules.mapEtaAbstraction,
        rules.splitJoin(32),
        // rules.splitJoin1M(32),
        rules.splitJoin2M(32),
        rules.reduceSeqMapFission,
        rules.undoReduceSeqForAdd
    ))
  }

  test("blocking partial 2") {
    val mm: Expr = tvmGemm.mm
    val start = tvmGemm.blockingPartial1(mm).get
    val goal = tvmGemm.blockingPartial2(mm).get

    ProveEquiv.init()
      .withFilter(ArrayDimensionPredicate(5) && ASTSizePredicate(200))
      .runBENF(start, goal, Seq(
        rules.mapFission,
        rules.reduceSeqMapFission,
        rules.undoReduceSeqForAdd
      ))
  }

  test("blocking partial 3") {
    val mm: Expr = tvmGemm.mm
    val start = tvmGemm.blockingPartial2(mm).get
    val goal = tvmGemm.blockingPartial3(mm).get

    ProveEquiv.init()
      .withFilter(ArrayDimensionPredicate(5) && ASTSizePredicate(200))
      .runBENF(start, goal, Seq(rules.blockedReduce(4)))
  }

  // FIXME: this times out since recent changes
  ignore("blocking reorder") {
    val mm: Expr = tvmGemm.mm
    val start = tvmGemm.blockingPartial3(mm).get
    val goal = tvmGemm.blocking(mm).get

    ProveEquiv.init()
      .runBENF(start, goal, Seq(
        // rules.eta,// rules.betaExtract, rules.betaNatExtract,
        rules.mapFission,
        rules.reduceSeq,
        rules.reduceSeqMapFusion,
        rules.undoReduceSeqForAdd,
        rules.splitBeforeMap,
        rules.liftReduceSeq,
        rules.liftReduceSeq2,
        rules.liftReduceSeq3
      ))
/*
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
 */
  }

  ignore("vectorize") {
    val mm: Expr = tvmGemm.mm
    val start = tvmGemm.blocking(mm).get
    val goal = tvmGemm.vectorization(mm).get

    ProveEquiv.init().runCNF(start, goal, Seq(
      // rules.eta, rules.betaExtract, rules.betaNatExtract,
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

  test("lift-reduce-seq 1") {
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
      rules.liftReduceSeq
    ))

    ProveEquiv.init().runCNF(start, goal, Seq(
      rules.combinatory.liftReduceSeq
    ))
  }

  test("lift-reduce-seq 2") {
    import rise.core.DSL._
    import rise.core.types._
    import rise.core.primitives._
    import rise.core.DSL.Type._

    val t = (8`.`16`.`32`.`64`.`(f32 x f32)) ->: (8`.`32`.`f32)

    val start: Expr = map(reduceSeq(fun(acc => fun(y =>
      map(fun(x =>
        fst(x) + reduceSeq(fun(acc => fun(y =>
          acc + (fst(y) * snd(y))
        )))(lf32(0))(snd(x))
      ))(zip(acc)(y))
    )))(generate(fun(_ => lf32(0))))) :: t

    val goal: Expr = fun(in => reduceSeq(fun(acc => fun(y =>
      map(fun(x =>
        map(fun(x =>
          fst(x) + reduceSeq(fun(acc => fun(y =>
            acc + (fst(y) * snd(y))
          )))(lf32(0))(snd(x))
        ))(zip(fst(x))(snd(x)))
      ))(zip(acc)(y))
    )))(generate(fun(_ => generate(fun(_ => lf32(0))))))(transpose(in))) :: t

    ProveEquiv.init().runBENF(start, goal, Seq(
      rules.liftReduceSeq
    ))

    ProveEquiv.init().runCNF(start, goal, Seq(
      rules.combinatory.liftReduceSeq
    ))
  }

  test("lift-reduce-seq 3") {
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
      map(fun(x => fst(x) + (fst(snd(x)) * snd(snd(x)))))(
        zip(acc)(y))
    )))(fst(unzip(in)))(transpose(snd(unzip(in))))) :: t

    ProveEquiv.init().runBENF(start, goal, Seq(
      rules.liftReduceSeq2
    ))

    ProveEquiv.init().runCNF(start, goal, Seq(
      rules.combinatory.liftReduceSeq2
    ))
  }

  test("lift-reduce-seq 4") {
    import rise.core.DSL._
    import rise.core.types._
    import rise.core.primitives._
    import rise.core.DSL.Type._

    val t = (8`.`16`.`(f32 x (32`.`(f32 x f32)))) ->: (8`.`16`.`f32)

    val start: Expr = map(fun(in => reduceSeq(fun(acc => fun(y =>
      map(fun(x => fst(x) + (fst(snd(x)) * snd(snd(x)))))(
        zip(acc)(y))
    )))(fst(unzip(in)))(transpose(snd(unzip(in)))))) :: t

    val goal: Expr = fun(in => reduceSeq(fun(acc => fun(y =>
      map(fun(x =>
        map(fun(x => fst(x) + (fst(snd(x)) * snd(snd(x)))))(zip(fst(x))(snd(x)))
      ))(zip(acc)(y))
    )))(fst(unzip(map(unzip)(in))))(transpose(map(transpose)(snd(unzip(map(unzip)(in))))))) :: t

    ProveEquiv.init().runBENF(start, goal, Seq(
      rules.liftReduceSeq3
    ))

    ProveEquiv.init().runCNF(start, goal, Seq(
      rules.combinatory.mapFission,
      rules.combinatory.mapFusion,
      rules.combinatory.mapFusion2,
      rules.combinatory.liftReduceSeq3
    ))
  }
}
