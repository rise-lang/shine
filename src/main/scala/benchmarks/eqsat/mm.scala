package benchmarks.eqsat

import rise.eqsat._
import rise.elevate.rules.lowering.lowerToC
import rise.elevate.rules.traversal.default._
import rise.eqsat.PredicateDSL._
import rise.eqsat.ExtendedPatternDSL._

object mm {
  val mm: rise.core.Expr = {
    import rise.core.DSL._
    import rise.core.types._
    import rise.core.primitives._

    depFun((m: Nat, n: Nat, k: Nat) =>
    fun(ArrayType(m, ArrayType(k, f32)))(a =>
    fun(ArrayType(k, ArrayType(n, f32)))(b =>
      a |> map(fun(ak =>
        transpose(b) |> map(fun(bk =>
          zip(ak)(bk) |>
            map(fun(x => fst(x) * snd(x))) |>
            reduce(add)(lf32(0.0f))
        ))
      ))
    )))
  }

  def containsMap(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(map :: `?t` ->: (n`.``?dt`) ->: `?t`, f))

  def containsMapSeq(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(mapSeq :: `?t` ->: (n`.``?dt`) ->: `?t`, f))

  def containsReduceSeq(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(reduceSeq :: `?t` ->: `?t` ->: (n`.``?dt`) ->: `?t`, f))

  val m = `%n`(2)
  val n = `%n`(1)
  val k = `%n`(0)

  val containsAddMul = contains(
    (app(app(add, ?), contains(mul)) : ExtendedPattern) or
    (contains(mul) >> contains(add))
  )

  val emptyStep = GuidedSearch.Step.init(BENF) /* withExtractor
    GuidedSearch.BeamExtractor(1, LexicographicCost(BENFRedexCount(), AstSize)) */

  val splitStepBENF = emptyStep withRules Seq(
    rules.mapFission,
    rules.reduceSeq,
    rules.eliminateMapIdentity,
    rules.reduceSeqMapFusion, //?
    rules.reduceSeqMapFission,
    rules.undoReduceSeqForAdd, //?
    // rules.mapEtaAbstraction,
    // TODO: could also restrict left-hand-side match to split
    //       e.g. only split "n"/"m" dimensions by 32 and "k" dimension by 4
    rules.splitJoin(32),
    // rules.splitJoin1M(32),
    rules.splitJoin2M(32),
    rules.blockedReduce(4),
    rules.splitBeforeMap,
  )

  val splitStepCNF = GuidedSearch.Step.init(CNF) withRules Seq(
    // rules.combinatory.mapFusion,
    // rules.combinatory.mapFusion2,
    // rules.combinatory.transposePairAfter,

    rules.combinatory.mapFission,
    rules.combinatory.mapFission2,
    rules.reduceSeq,
    // rules.eliminateMapIdentity, //?
    rules.combinatory.reduceSeqMapFusion, //?
    rules.combinatory.reduceSeqMapFusion2,
    rules.combinatory.reduceSeqMapFission,
    rules.undoReduceSeqForAdd, //?
    // rules.mapEtaAbstraction,
    rules.combinatory.splitJoin(32),
    // rules.splitJoin1M(32),
    rules.combinatory.splitJoin2M(32),
    rules.combinatory.blockedReduce(4),
    rules.combinatory.splitBeforeMap,
    rules.combinatory.splitBeforeMap2,
  )

  val reorderStepBENF = emptyStep withRules Seq(
    rules.mapFission,
    // rules.reduceSeq,
    rules.reduceSeqMapFusion,
    rules.reduceSeqMapFission,
    rules.eliminateMapIdentity,
    // rules.undoReduceSeqForAdd, //?
    rules.splitBeforeMap,
    rules.liftReduceSeq,
    rules.liftReduceSeq2,
    rules.liftReduceSeq3,
    // rules.transposeAroundMapMapF,
    rules.transposeAroundMapMapF1M,
    // rules.mapEtaAbstraction,
  )

  val reorderStepCNF = GuidedSearch.Step.init(CNF) withRules Seq(
    // rules.combinatory.mapFusion,
    // rules.combinatory.mapFusion2,

    rules.combinatory.mapFission,
    rules.combinatory.mapFission2,
    rules.combinatory.reduceSeqMapFusion,
    rules.combinatory.reduceSeqMapFusion2,
    rules.combinatory.reduceSeqMapFission,
    rules.eliminateMapIdentity, //?
    // rules.undoReduceSeqForAdd, //?
    rules.combinatory.splitBeforeMap,
    rules.combinatory.splitBeforeMap2,
    rules.combinatory.liftReduceSeq,
    rules.combinatory.liftReduceSeq2,
    rules.combinatory.liftReduceSeq3,
    // rules.transposeAroundMapMapF,
    rules.combinatory.transposeAroundMapMapF1M,
    // rules.mapEtaAbstraction,
  )

  val tilingStepBENF = splitStepBENF compose reorderStepBENF

  val loweringStep = GuidedSearch.Step.init(BENF) withRules Seq(
    rules.mapFusion,
    rules.reduceSeq,
    rules.mapSeq,
    rules.mapSeqArray,
  ) // TODO: withExtractor ???

  private def blockingGoal(): () = {
    val goal = apps.tvmGemm.blocking(mm).get
    val normGoal = BENF(Expr.fromNamed(goal))
    val goalSize = AstSize.ofExpr(normGoal)
    println(s"normalized goal: ${Expr.toNamed(normGoal)}")
    println(s"goal size: ${goalSize}")

    val loweredGoal = lowerToC.apply(goal).get
    val goalCode = util.gen.c.function.asStringFromExpr(loweredGoal)
    util.writeToPath("/tmp/goal.c", goalCode)
  }

  private def codegen(name: String, e: Expr): () = {
    // val loweredWithElevate = lowerToC.apply(Expr.toNamed(e)).get
    LoweringSearch.init().run(BENF, AstSize, Seq(e), Seq(
      rules.mapFusion,
      rules.reduceSeq,
      rules.mapSeq,
      rules.mapSeqArray,
    )) match {
      case Some(loweredWithEqsat) =>
        val code = util.gen.c.function.asStringFromExpr(Expr.toNamed(loweredWithEqsat))
        util.writeToPath(s"/tmp/${name.replace(' ', '_')}.c", code)
      case None => println("could not generate code")
    }
    // println(loweredWithElevate)
    // println(loweredWithEqsat)
  }

  private def blocking_T(tilingStep: GuidedSearch.Step): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get

    val steps = Seq(
      emptyStep withSketch
        containsMap(m,
          containsMap(n,
            containsReduceSeq(k, containsAddMul))),
      tilingStep withSketch
        containsMap(m /^ cst(32),
          containsMap(n /^ cst(32),
            containsReduceSeq(k /^ cst(4),
              containsReduceSeq(cst(4),
                containsMap(cst(32),
                  containsMap(cst(32), containsAddMul)))))),
    )

    GuidedSearch.init()
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(200) &&
        StandardConstraintsPredicate)
      .withRunnerTransform(r =>
        r.withTimeLimit(java.time.Duration.ofMinutes(10))
         .withMemoryLimit(4L * 1024L * 1024L * 1024L /* 4GiB */)
         .withNodeLimit(2_000_000))
      .run(start, steps)
  }

  private def blocking_TTTT(tilingStep: GuidedSearch.Step): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get

    val steps = Seq(
      emptyStep withSketch
        containsMap(m,
          containsMap(n,
            containsReduceSeq(k, containsAddMul))),
      tilingStep withSketch
        containsMap(m /^ cst(32),
          containsMap(cst(32),
            containsMap(n /^ cst(32),
              containsMap(cst(32),
                containsReduceSeq(k, containsAddMul))))),
      tilingStep withSketch
        containsMap(m /^ cst(32),
          containsMap(n /^ cst(32),
            containsMap(cst(32),
              containsMap(cst(32),
                containsReduceSeq(k, containsAddMul))))),
      tilingStep withSketch
        containsMap(m /^ cst(32),
          containsMap(n /^ cst(32),
            containsMap(cst(32),
              containsMap(cst(32),
                containsReduceSeq(k /^ cst(4),
                  containsReduceSeq(cst(4), containsAddMul)))))),
      tilingStep withSketch
        containsMap(m /^ cst(32),
          containsMap(n /^ cst(32),
            containsReduceSeq(k /^ cst(4),
              containsReduceSeq(cst(4),
                containsMap(cst(32),
                  containsMap(cst(32), containsAddMul)))))),
    )

    GuidedSearch.init()
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(200) &&
        StandardConstraintsPredicate)
      .withRunnerTransform(r =>
        r.withTimeLimit(java.time.Duration.ofMinutes(5))
          .withMemoryLimit(4L * 1024L * 1024L * 1024L /* 4GiB */))
      .run(start, steps)
  }

  private def blocking_SRSR(splitStep: GuidedSearch.Step,
                            reorderStep: GuidedSearch.Step): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get

    val steps = Seq(
      emptyStep withSketch
        containsMap(m,
          containsMap(n,
            containsReduceSeq(k, containsAddMul))),
      splitStep withSketch
        containsMap(m /^ cst(32),
          containsMap(cst(32),
            containsMap(n /^ cst(32),
              containsMap(cst(32),
                containsReduceSeq(k, containsAddMul))))),
      reorderStep withSketch
        containsMap(m /^ cst(32),
          containsMap(n /^ cst(32),
            containsMap(cst(32),
              containsMap(cst(32),
                containsReduceSeq(k, containsAddMul))))),
      splitStep withSketch
        containsMap(m /^ cst(32),
          containsMap(n /^ cst(32),
            containsMap(cst(32),
              containsMap(cst(32),
                containsReduceSeq(k /^ cst(4),
                  containsReduceSeq(cst(4), containsAddMul)))))),
      reorderStep withSketch
        containsMap(m /^ cst(32),
          containsMap(n /^ cst(32),
            containsReduceSeq(k /^ cst(4),
              containsReduceSeq(cst(4),
                containsMap(cst(32),
                  containsMap(cst(32), containsAddMul)))))),
    )

    GuidedSearch.init()
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(200) &&
        StandardConstraintsPredicate)
      .withRunnerTransform(r =>
        r.withTimeLimit(java.time.Duration.ofMinutes(5))
          .withMemoryLimit(4L * 1024L * 1024L * 1024L /* 4GiB */))
      .run(start, steps)
  }

  private def blocking_TT(tilingStep: GuidedSearch.Step): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get

    val steps = Seq(
      emptyStep withSketch
        containsMap(m,
          containsMap(n,
            containsReduceSeq(k, containsAddMul))),
      tilingStep withSketch
        containsMap(m /^ cst(32),
          containsMap(cst(32),
            containsMap(n /^ cst(32),
              containsMap(cst(32),
                containsReduceSeq(k /^ cst(4),
                  containsReduceSeq(cst(4), containsAddMul)))))),
      tilingStep withSketch
        containsMap(m /^ cst(32),
          containsMap(n /^ cst(32),
            containsReduceSeq(k /^ cst(4),
              containsReduceSeq(cst(4),
                containsMap(cst(32),
                  containsMap(cst(32), containsAddMul)))))),
    )

    GuidedSearch.init()
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(200) &&
        StandardConstraintsPredicate)
      .withRunnerTransform(r =>
        r.withTimeLimit(java.time.Duration.ofMinutes(5))
          .withMemoryLimit(4L * 1024L * 1024L * 1024L /* 4GiB */))
      .run(start, steps)
  }

  private def blocking_SR(splitStep: GuidedSearch.Step,
                          reorderStep: GuidedSearch.Step): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get

    val steps = Seq(
      emptyStep withSketch
        containsMap(m,
          containsMap(n,
            containsReduceSeq(k, containsAddMul))),
      splitStep withSketch
        containsMap(m /^ cst(32),
          containsMap(cst(32),
            containsMap(n /^ cst(32),
              containsMap(cst(32),
                containsReduceSeq(k /^ cst(4),
                  containsReduceSeq(cst(4), containsAddMul)))))),
      reorderStep withSketch
        containsMap(m /^ cst(32),
          containsMap(n /^ cst(32),
            containsReduceSeq(k /^ cst(4),
              containsReduceSeq(cst(4),
                containsMap(cst(32),
                  containsMap(cst(32), containsAddMul)))))),
    )

    GuidedSearch.init()
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(200) &&
        StandardConstraintsPredicate)
      .withRunnerTransform(r =>
        r.withTimeLimit(java.time.Duration.ofMinutes(5))
          .withMemoryLimit(4L * 1024L * 1024L * 1024L /* 4GiB */))
      .run(start, steps)
  }

  // TODO:
  // tvmGemm.vectorization(mm).get,
  // tvmGemm.loopPerm(mm).get,
  // tvmGemm.arrayPacking(mm).get,
  // tvmGemm.cacheBlocks(mm).get,
  // tvmGemm.par(mm).get

  def main(args: Array[String]): () = {
    val fs = Seq(
      // not found after 3mn+ and 2GiB+ (700K nodes, 400K classes)
      // "blocking T" -> blocking_T _,
      // "blocking TTTT" -> { () => blocking_TTTT(tilingStepBENF) },
      // "blocking SRSR" -> { () => blocking_SRSR(splitStepBENF, reorderStepBENF) },
      // FIXME: the program found has unwanted split/joins
      // "blocking TT" -> { () => blocking_TT(tilingStepBENF) },
      "blocking SR" -> { () => blocking_SR(splitStepBENF, reorderStepBENF) },
      // FIXME: cannot find goal, rewriting is stuck with the given rules
      // "blocking SR CNF" -> { () => blocking_SR(splitStepCNF, reorderStepCNF) },
    )
    val rs = fs.map { case (n, f) =>
      (n, util.time(f()))
    }
    rs.foreach { case (n, (_, r)) =>
      r.exprs.headOption.foreach(codegen(n, _))
    }
    blockingGoal()
    rs.foreach { case (n, (t, r)) =>
      println(s"-------- $n")
      val status = if (r.exprs.nonEmpty) { "found" } else { "not found" }
      println(s"$status after ${util.prettyTime(t)}")
      r.printReport()
    }

    // -------- blocking TTTT
    // found after 1mn 41s (1mn 4s with constant sizes)
    // -------- blocking SRSR
    // found after 5s (3s with constant sizes)
    // -------- blocking TT
    // found after 1mn 49s (53s with constant sizes)
    // -------- blocking SR
    // found after 14s (7s with constant sizes)
  }
}
