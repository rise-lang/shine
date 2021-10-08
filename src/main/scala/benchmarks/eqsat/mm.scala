package benchmarks.eqsat

import rise.eqsat._
import rise.elevate.rules.lowering.lowerToC
import rise.elevate.rules.traversal.default._
import rise.eqsat.PredicateDSL._
import rise.eqsat.ExtendedPatternDSL._

object mm {
  val M = 4096
  val N = 2048
  val K = 1024
  val mm: rise.core.Expr = {
    import rise.core.DSL._
    import rise.core.types._
    import rise.core.primitives._

    fun(ArrayType(M, ArrayType(K, f32)))(a =>
      fun(ArrayType(K, ArrayType(N, f32)))(b =>
        a |> map(fun(ak =>
          transpose(b) |> map(fun(bk =>
            zip(ak)(bk) |>
              map(fun(x => fst(x) * snd(x))) |>
              reduce(add)(lf32(0.0f))
          ))
        ))
      ))
  }

  def containsMap(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(map :: `?t` ->: (n`.``?dt`) ->: `?t`, f))

  def containsMapSeq(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(mapSeq :: `?t` ->: (n`.``?dt`) ->: `?t`, f))

  def containsReduceSeq(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(reduceSeq :: `?t` ->: `?t` ->: (n`.``?dt`) ->: `?t`, f))

  val m = cst(M) // `?n`(0)
  val n = cst(N) // `?n`(1)
  val k = cst(K) // `?n`(2)
  // TODO: nat pivot matching
  val m_div32 = cst(M / 32)
  val n_div32 = cst(N / 32)
  val k_div4 = cst(K / 4)

  val containsAddMul = contains(app(app(add, ?), contains(mul)))

  val emptyStep = GuidedSearch.Step.init(BENF)

  val splitStep = GuidedSearch.Step.init(BENF) withRules Seq(
    rules.mapFission,
    rules.reduceSeq,
    rules.eliminateMapIdentity,
    rules.reduceSeqMapFusion, //?
    rules.reduceSeqMapFission,
    rules.undoReduceSeqForAdd, //?
    // rules.splitBeforeMap,
    // rules.mapEtaAbstraction,
    rules.splitJoin(32),
    // rules.splitJoin1M(32),
    rules.splitJoin2M(32),
    rules.blockedReduce(4),
    rules.splitBeforeMap,
  )

  val reorderStep = GuidedSearch.Step.init(BENF) withRules Seq(
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

  val tilingStep = splitStep compose reorderStep

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
    val named = Expr.toNamed(e)
    // try {
      // TODO: use search for this
      val loweredWithElevate = lowerToC.apply(named).get
      val loweredWithEqsat = Expr.toNamed(LoweringSearch.init().run(BENF, AstSize, Seq(e), Seq(
        rules.mapFusion,
        rules.reduceSeq,
        rules.mapSeq,
        rules.mapSeqArray,
      )))
      println(loweredWithElevate)
      println(loweredWithEqsat)

      val code = util.gen.c.function.asStringFromExpr(loweredWithEqsat)
      util.writeToPath(s"/tmp/${name.replace(' ', '_')}.c", code)
    /* } catch {
      case e: Exception => println(e)
    } */
  }

  private def blocking_T(): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get

    val steps = Seq(
      emptyStep withSketch
        containsMap(m,
          containsMap(n,
            containsReduceSeq(k, containsAddMul))),
      tilingStep withSketch
        containsMap(/*m /^ cst(32)*/m_div32,
          containsMap(/*n /^ cst(32)*/n_div32,
            containsReduceSeq(/*k /^ cst(4)*/k_div4,
              containsReduceSeq(cst(4),
                containsMap(cst(32),
                  containsMap(cst(32), containsAddMul)))))),
      /* lowerToC
      loweringSearch ->
        containsMapSeq(/*m /^ cst(32)*/m_div32,
          containsMapSeq(/*n /^ cst(32)*/n_div32,
            containsReduceSeq(/*k /^ cst(4)*/k_div4,
              containsReduceSeq(cst(4),
                containsMapSeq(cst(32),
                  containsMapSeq(cst(32), ?))))))

       */
    )

    GuidedSearch.init()
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(200) &&
        StandardConstraintsPredicate)
      .withRunnerTransform(r =>
        r.withTimeLimit(java.time.Duration.ofMinutes(10))
         .withMemoryLimit(2L * 1024L * 1024L * 1024L /* 2GiB */)
         .withNodeLimit(2_000_000))
      .run(start, steps)
  }

  private def blocking_TTTT(): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get

    val steps = Seq(
      emptyStep withSketch
        containsMap(m,
          containsMap(n,
            containsReduceSeq(k, containsAddMul))),
      tilingStep withSketch
        containsMap(/*m /^ cst(32)*/m_div32,
          containsMap(cst(32),
            containsMap(/*n /^ cst(32)*/n_div32,
              containsMap(cst(32),
                containsReduceSeq(k, containsAddMul))))),
      tilingStep withSketch
        containsMap(/*m /^ cst(32)*/m_div32,
          containsMap(/*n /^ cst(32)*/n_div32,
            containsMap(cst(32),
              containsMap(cst(32),
                containsReduceSeq(k, containsAddMul))))),
      tilingStep withSketch
        containsMap(/*m /^ cst(32)*/m_div32,
          containsMap(/*n /^ cst(32)*/n_div32,
            containsMap(cst(32),
              containsMap(cst(32),
                containsReduceSeq(/*k /^ cst(4)*/k_div4,
                  containsReduceSeq(cst(4), containsAddMul)))))),
      tilingStep withSketch
        containsMap(/*m /^ cst(32)*/m_div32,
          containsMap(/*n /^ cst(32)*/n_div32,
            containsReduceSeq(/*k /^ cst(4)*/k_div4,
              containsReduceSeq(cst(4),
                containsMap(cst(32),
                  containsMap(cst(32), containsAddMul)))))),
      /* lowerToC
      loweringSearch ->
        containsMapSeq(/*m /^ cst(32)*/m_div32,
          containsMapSeq(/*n /^ cst(32)*/n_div32,
            containsReduceSeq(/*k /^ cst(4)*/k_div4,
              containsReduceSeq(cst(4),
                containsMapSeq(cst(32),
                  containsMapSeq(cst(32), ?))))))

       */
    )

    GuidedSearch.init()
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(200) &&
        StandardConstraintsPredicate)
      .withRunnerTransform(r =>
        r.withTimeLimit(java.time.Duration.ofMinutes(5)))
      .run(start, steps)
  }

  private def blocking_SRSR(): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get

    val steps = Seq(
      emptyStep withSketch
        containsMap(m,
          containsMap(n,
            containsReduceSeq(k, containsAddMul))),
      splitStep withSketch
        containsMap(/*m /^ cst(32)*/m_div32,
          containsMap(cst(32),
            containsMap(/*n /^ cst(32)*/n_div32,
              containsMap(cst(32),
                containsReduceSeq(k, containsAddMul))))),
      reorderStep withSketch
        containsMap(/*m /^ cst(32)*/m_div32,
          containsMap(/*n /^ cst(32)*/n_div32,
            containsMap(cst(32),
              containsMap(cst(32),
                containsReduceSeq(k, containsAddMul))))),
      splitStep withSketch
        containsMap(/*m /^ cst(32)*/m_div32,
          containsMap(/*n /^ cst(32)*/n_div32,
            containsMap(cst(32),
              containsMap(cst(32),
                containsReduceSeq(/*k /^ cst(4)*/k_div4,
                  containsReduceSeq(cst(4), containsAddMul)))))),
      reorderStep withSketch
        containsMap(/*m /^ cst(32)*/m_div32,
          containsMap(/*n /^ cst(32)*/n_div32,
            containsReduceSeq(/*k /^ cst(4)*/k_div4,
              containsReduceSeq(cst(4),
                containsMap(cst(32),
                  containsMap(cst(32), containsAddMul)))))),
      /* lowerToC
      loweringSearch ->
        containsMapSeq(/*m /^ cst(32)*/m_div32,
          containsMapSeq(/*n /^ cst(32)*/n_div32,
            containsReduceSeq(/*k /^ cst(4)*/k_div4,
              containsReduceSeq(cst(4),
                containsMapSeq(cst(32),
                  containsMapSeq(cst(32), ?))))))

       */
    )

    GuidedSearch.init()
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(200) &&
        StandardConstraintsPredicate)
      .withRunnerTransform(r =>
        r.withTimeLimit(java.time.Duration.ofMinutes(5)))
      .run(start, steps)
  }

  private def blocking_TT(): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get

    val steps = Seq(
      emptyStep withSketch
        containsMap(m,
          containsMap(n,
            containsReduceSeq(k, containsAddMul))),
      tilingStep withSketch
        containsMap(/*m /^ cst(32)*/m_div32,
          containsMap(cst(32),
            containsMap(/*n /^ cst(32)*/n_div32,
              containsMap(cst(32),
                containsReduceSeq(/*k /^ cst(4)*/k_div4,
                  containsReduceSeq(cst(4), containsAddMul)))))),
      tilingStep withSketch
        containsMap(/*m /^ cst(32)*/m_div32,
          containsMap(/*n /^ cst(32)*/n_div32,
            containsReduceSeq(/*k /^ cst(4)*/k_div4,
              containsReduceSeq(cst(4),
                containsMap(cst(32),
                  containsMap(cst(32), containsAddMul)))))),
      /* lowerToC
      loweringSearch ->
        containsMapSeq(/*m /^ cst(32)*/m_div32,
          containsMapSeq(/*n /^ cst(32)*/n_div32,
            containsReduceSeq(/*k /^ cst(4)*/k_div4,
              containsReduceSeq(cst(4),
                containsMapSeq(cst(32),
                  containsMapSeq(cst(32), ?))))))

       */
    )

    GuidedSearch.init()
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(200) &&
        StandardConstraintsPredicate)
      .withRunnerTransform(r =>
        r.withTimeLimit(java.time.Duration.ofMinutes(5)))
      .run(start, steps)
  }

  private def blocking_SR(): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get

    val steps = Seq(
      emptyStep withSketch
        containsMap(m,
          containsMap(n,
            containsReduceSeq(k, containsAddMul))),
      splitStep withSketch
        containsMap(/*m /^ cst(32)*/m_div32,
          containsMap(cst(32),
            containsMap(/*n /^ cst(32)*/n_div32,
              containsMap(cst(32),
                containsReduceSeq(/*k /^ cst(4)*/k_div4,
                  containsReduceSeq(cst(4), containsAddMul)))))),
      reorderStep withSketch
        containsMap(/*m /^ cst(32)*/m_div32,
          containsMap(/*n /^ cst(32)*/n_div32,
            containsReduceSeq(/*k /^ cst(4)*/k_div4,
              containsReduceSeq(cst(4),
                containsMap(cst(32),
                  containsMap(cst(32), containsAddMul)))))),
      /* lowerToC
      loweringSearch ->
        containsMapSeq(/*m /^ cst(32)*/m_div32,
          containsMapSeq(/*n /^ cst(32)*/n_div32,
            containsReduceSeq(/*k /^ cst(4)*/k_div4,
              containsReduceSeq(cst(4),
                containsMapSeq(cst(32),
                  containsMapSeq(cst(32), ?))))))

       */
    )

    GuidedSearch.init()
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(200) &&
        StandardConstraintsPredicate)
      .withRunnerTransform(r =>
        r.withTimeLimit(java.time.Duration.ofMinutes(5)))
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
      // max dim 5: not found after 10mn+ and 14GiB+ (1M nodes, 500K classes)
      // TODO: max dim 6
      // "blocking T" -> blocking_T _,
      // "blocking TTTT" -> blocking_TTTT _,
      // "blocking SRSR" -> blocking_SRSR _,
      // "blocking TT" -> blocking_TT _, // FIXME: why is the program found with useless split/join?
      "blocking SR" -> blocking_SR _,
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
    // found after 1mn 6s 628ms 495µs
    // -------- blocking SRSR
    // found after 2s 983ms 653µs
    // -------- blocking TT
    // found after 53s 165ms 364µs
    // -------- blocking SR
    // found after 7s 636ms 242µs
  }
}
