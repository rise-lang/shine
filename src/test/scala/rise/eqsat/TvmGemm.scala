package rise.eqsat

import rise.core.Expr
import rise.elevate.tvmGemm
import ProveEquiv.syntax._
import rise.eqsat.PredicateDSL._

class TvmGemm extends test_util.Tests {
  test("baseline") {
    val mm: Expr = tvmGemm.mm
    val goal = tvmGemm.baseline(mm).get

    ProveEquiv.init().runBENF(mm, goal, Seq(
      rules.eta, rules.betaExtract, rules.betaNatExtract,
      rules.reduceSeq, rules.reduceSeqMapFusion
    ))
  }

  // tvmGemm.vectorization(mm).get,
  // tvmGemm.loopPerm(mm).get,
  // tvmGemm.arrayPacking(mm).get,
  // tvmGemm.cacheBlocks(mm).get,
  // tvmGemm.par(mm).get
  test("blocking") {
    val mm: Expr = tvmGemm.mm
    val start = tvmGemm.baseline(mm).get
    val goal = tvmGemm.blocking(mm).get

    val rs = Seq(
      rules.eta, rules.betaExtract, rules.betaNatExtract,
      rules.mapFission,
      rules.reduceSeq,
      rules.reduceSeqMapFusion,
      rules.reduceSeqMapFission,
      rules.undoReduceSeqForAdd, //?
      rules.splitBeforeMap,
      rules.liftReduceSeq,
      rules.liftReduceSeq2,
      rules.liftReduceSeq3,
      // rules.transposeAroundMapMapF,
      rules.transposeAroundMapMapF1M,
      // rules.mapEtaAbstraction,
      rules.splitJoin(32),
      // rules.splitJoin1M(32),
      rules.splitJoin2M(32),
      rules.blockedReduce(4),
    )

    val rewriteSnapshots = {
      import ExtendedPatternDSL._

      def containsMap(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
        contains(app(map :: `?t` ->: (n`.``?dt`) ->: `?t`, f))
      def containsReduce(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
        contains(app(reduceSeq :: `?t` ->: `?t` ->: (n`.``?dt`) ->: `?t`, f))

      val m = cst(1024) // `?n`(0)
      val n = cst(1024) // `?n`(1)
      val k = cst(1024) // `?n`(2)
      // TODO: nat pivot matching
      val div32 = cst(1024 / 32)
      val div4 = cst(1024 / 4)
      Seq(
        containsMap(m,
          containsMap(n,
            containsReduce(k, ?))),
        containsMap(/*m /^ cst(32)*/div32,
          containsMap(cst(32),
            containsMap(/*n /^ cst(32)*/div32,
              containsMap(cst(32),
                containsReduce(k, ?))))),
        containsMap(/*m /^ cst(32)*/div32,
          containsMap(/*n /^ cst(32)*/div32,
            containsMap(cst(32),
              containsMap(cst(32),
                containsReduce(k, ?))))),
        containsMap(/*m /^ cst(32)*/div32,
          containsMap(/*n /^ cst(32)*/div32,
            containsMap(cst(32),
              containsMap(cst(32),
                containsReduce(/*k /^ cst(4)*/div4,
                  containsReduce(cst(4), ?)))))),
        containsMap(/*m /^ cst(32)*/div32,
          containsMap(/*n /^ cst(32)*/div32,
            containsReduce(/*k /^ cst(4)*/div4,
              containsReduce(cst(4),
                containsMap(cst(32),
                  containsMap(cst(32), ?))))))
      )
    }

    GuidedSearch.init()
      .withFilter(ArrayDimensionPredicate(5) && ASTSizePredicate(120))
      .runBENF(start, goal, rs, rewriteSnapshots)
  }

  test("blocking partial") {
    val mm: Expr = tvmGemm.mm
    val start = tvmGemm.baseline(mm).get
    val goals = Seq(
      tvmGemm.blockingPartial(mm).get,
      tvmGemm.blockingPartial2(mm).get,
      tvmGemm.blockingPartial3(mm).get
    )

    ProveEquiv.init()
      .withFilter(ArrayDimensionPredicate(5) && ASTSizePredicate(80))
      .runBENF(start, goals, Seq(
        rules.eta, rules.betaExtract, rules.betaNatExtract,
        rules.mapFission,
        // rules.transposeAroundMapMapF,
        rules.transposeAroundMapMapF1M,
        // rules.mapEtaAbstraction,
        rules.splitJoin(32),
        // rules.splitJoin1M(32),
        rules.splitJoin2M(32),
        rules.reduceSeqMapFission,
        rules.undoReduceSeqForAdd,
        rules.blockedReduce(4),
    ))
/*
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
 */
  }

  test("blocking reorder") {
    val mm: Expr = tvmGemm.mm
    val start = tvmGemm.blockingPartial3(mm).get
    val goal = tvmGemm.blocking(mm).get

    ProveEquiv.init()
      .runBENF(start, goal, Seq(
        rules.eta, rules.betaExtract, rules.betaNatExtract,
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
      rules.eta, rules.betaExtract, rules.betaNatExtract,
      rules.liftReduceSeq
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
      rules.eta, rules.betaExtract, rules.betaNatExtract,
      rules.liftReduceSeq2
    ))
/*
    ProveEquiv.init().runCNF(start, goal, Seq(
      rules.eta, rules.betaExtract, rules.betaNatExtract,
      rules.combinatory.compositionIntro,
      rules.combinatory.compositionElim,
      rules.combinatory.compositionAssoc1,
      rules.combinatory.compositionAssoc2,
      rules.combinatory.liftReduceSeq2
    ))
 */
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
      rules.eta, rules.betaExtract, rules.betaNatExtract,
      rules.liftReduceSeq3
    ))
  }
}
