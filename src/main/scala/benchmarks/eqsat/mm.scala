package benchmarks.eqsat

import rise.eqsat._
import rise.elevate.rules.lowering.lowerToC
import rise.elevate.rules.traversal.default._
import elevate.core.Then
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

  val noSearch = Seq() -> AstSize

  val splitSearch = Seq(
    rules.mapFission,
    rules.reduceSeq,
    rules.eliminateMapIdentity,
    // rules.reduceSeqMapFusion, //?
    rules.reduceSeqMapFission,
    rules.undoReduceSeqForAdd, //?
    // rules.splitBeforeMap,
    // rules.mapEtaAbstraction,
    rules.splitJoin(32),
    // rules.splitJoin1M(32),
    rules.splitJoin2M(32),
    rules.blockedReduce(4),
  ) -> AstSize

  val reorderSearch = Seq(
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
  ) -> AstSize

  val tilingSearch =
    (splitSearch._1 ++ reorderSearch._1).distinctBy(_.name) -> AstSize

  val loweringSearch = Seq(
    rules.mapFusion,
    rules.reduceSeq,
    rules.mapSeq
  ) -> AstSize

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
    println(named)
    // TODO: use search for this
    val lowered = lowerToC.apply(named).get

    val code = util.gen.c.function.asStringFromExpr(lowered)
    util.writeToPath(s"/tmp/${name.replace(' ', '_')}.c", code)
  }

  private def blocking_4steps_1configs(): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get

    val m = cst(M) // `?n`(0)
    val n = cst(N) // `?n`(1)
    val k = cst(K) // `?n`(2)
    // TODO: nat pivot matching
    val m_div32 = cst(M / 32)
    val n_div32 = cst(N / 32)
    val k_div4 = cst(K / 4)

    val containsAddMul = contains(app(app(add, ?), contains(mul)))

    val sketches = Seq(
      noSearch ->
        containsMap(m,
          containsMap(n,
            containsReduceSeq(k, containsAddMul))),
      tilingSearch ->
        containsMap(/*m /^ cst(32)*/m_div32,
          containsMap(cst(32),
            containsMap(/*n /^ cst(32)*/n_div32,
              containsMap(cst(32),
                containsReduceSeq(k, containsAddMul))))),
      tilingSearch ->
        containsMap(/*m /^ cst(32)*/m_div32,
          containsMap(/*n /^ cst(32)*/n_div32,
            containsMap(cst(32),
              containsMap(cst(32),
                containsReduceSeq(k, containsAddMul))))),
      tilingSearch ->
        containsMap(/*m /^ cst(32)*/m_div32,
          containsMap(/*n /^ cst(32)*/n_div32,
            containsMap(cst(32),
              containsMap(cst(32),
                containsReduceSeq(/*k /^ cst(4)*/k_div4,
                  containsReduceSeq(cst(4), contains(add))))))),
      tilingSearch ->
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
      .withFilter(ArrayDimensionPredicate(5) && ASTSizePredicate(200) &&
        StandardConstraintsPredicate)
      .withRunnerTransform(r =>
        r.withTimeLimit(java.time.Duration.ofMinutes(5)))
      .runBENF(start, sketches)
  }

  private def blocking_4steps_2configs(): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get

    val m = cst(M) // `?n`(0)
    val n = cst(N) // `?n`(1)
    val k = cst(K) // `?n`(2)
    // TODO: nat pivot matching
    val m_div32 = cst(M / 32)
    val n_div32 = cst(N / 32)
    val k_div4 = cst(K / 4)

    val containsAddMul = contains(app(app(add, ?), contains(mul)))

    val sketches = Seq(
      noSearch ->
        containsMap(m,
          containsMap(n,
            containsReduceSeq(k, containsAddMul))),
      splitSearch ->
        containsMap(/*m /^ cst(32)*/m_div32,
          containsMap(cst(32),
            containsMap(/*n /^ cst(32)*/n_div32,
              containsMap(cst(32),
                containsReduceSeq(k, containsAddMul))))),
      reorderSearch ->
        containsMap(/*m /^ cst(32)*/m_div32,
          containsMap(/*n /^ cst(32)*/n_div32,
            containsMap(cst(32),
              containsMap(cst(32),
                containsReduceSeq(k, containsAddMul))))),
      splitSearch ->
        containsMap(/*m /^ cst(32)*/m_div32,
          containsMap(/*n /^ cst(32)*/n_div32,
            containsMap(cst(32),
              containsMap(cst(32),
                containsReduceSeq(/*k /^ cst(4)*/k_div4,
                  containsReduceSeq(cst(4), contains(add))))))),
      reorderSearch ->
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
      .withFilter(ArrayDimensionPredicate(5) && ASTSizePredicate(200) &&
        StandardConstraintsPredicate)
      .withRunnerTransform(r =>
        r.withTimeLimit(java.time.Duration.ofMinutes(5)))
      .runBENF(start, sketches)
  }

  // TODO:
  // tvmGemm.vectorization(mm).get,
  // tvmGemm.loopPerm(mm).get,
  // tvmGemm.arrayPacking(mm).get,
  // tvmGemm.cacheBlocks(mm).get,
  // tvmGemm.par(mm).get

  def main(args: Array[String]): () = {
    val fs = Seq(
      "blocking 4steps 2configs" -> blocking_4steps_2configs _,
      "blocking 4steps 1configs" -> blocking_4steps_1configs _
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
  }
}
