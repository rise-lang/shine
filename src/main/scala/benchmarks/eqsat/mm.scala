package benchmarks.eqsat

import rise.core.{types => rct}
import rise.eqsat._
import rise.elevate.rules.lowering.lowerToC
import rise.elevate.rules.traversal.default._
import rise.eqsat.PredicateDSL._
import rise.eqsat.ExtendedPatternDSL._

object mm {
  val mm: rise.core.DSL.ToBeTyped[rise.core.Expr] = {
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
  def containsMap(dt: DataTypePattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(map :: `?t` ->: dt ->: `?t`, f))
  def containsMap(n: NatPattern, f: ExtendedPattern, in: ExtendedPattern): ExtendedPattern =
    contains(app(app(map :: `?t` ->: (n`.``?dt`) ->: `?t`, f), in))
  def containsMapPar(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(omp.mapPar :: `?t` ->: (n`.``?dt`) ->: `?t`, f))
  def containsMapPar(dt: DataTypePattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(omp.mapPar :: `?t` ->: dt ->: `?t`, f))
  def containsMapPar(n: NatPattern, f: ExtendedPattern, in: ExtendedPattern): ExtendedPattern =
    contains(app(app(omp.mapPar :: `?t` ->: (n`.``?dt`) ->: `?t`, f), in))

  def containsMapSeq(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(mapSeq :: `?t` ->: (n`.``?dt`) ->: `?t`, f))

  def containsReduceSeq(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(reduceSeq :: `?t` ->: `?t` ->: (n`.``?dt`) ->: `?t`, f))
  def containsReduceSeqUnroll(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(reduceSeqUnroll :: `?t` ->: `?t` ->: (n`.``?dt`) ->: `?t`, f))

  val m = `%n`(2)
  val n = `%n`(1)
  val k = `%n`(0)

  val containsAddMul = contains(
    (app(app(add, ?), contains(mul)) : ExtendedPattern)/* or
    (contains(mul) >> contains(add))*/
  )
  val containsAddMulVec = contains(
    app(app(add, ? :: vecT(`?n`, `?dt`)), contains(mul)) :: vecT(`?n`, `?dt`)
  )

  val emptyStep = GuidedSearch.Step.init(BENF) /* withExtractor
    GuidedSearch.BeamExtractor(1, LexicographicCost(BENFRedexCount(), AstSize)) */

  val splitStepBENF = emptyStep withRules Seq(
    rules.mapFission,
    rules.reduceSeq,
    rules.eliminateMapIdentity,
    rules.reduceSeqMapFusion,
    rules.reduceSeqMapFission,
    rules.undoReduceSeqForAdd, //?
    // rules.mapEtaAbstraction,
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

  val copyStep = GuidedSearch.Step.init(BENF) withRules Seq(
    rules.storeToMem,
    rules.splitJoin2(32),
    rules.mapArray,
    rules.transposeAroundMapMapF1M,
  )

  val loweringStep = GuidedSearch.Step.init(BENF) withRules Seq(
    rules.mapFusion,
    rules.mapEtaAbstraction,
    rules.vectorize.after(32, rct.f32),
    rules.vectorize.after(32, rct.PairType(rct.f32, rct.f32)),
    rules.vectorize.after(32, rct.PairType(rct.f32, rct.PairType(rct.f32, rct.f32))),
    rules.vectorize.beforeMapF32,
    rules.vectorize.beforeMap_F32xF32,
    rules.vectorize.beforeMap_F32x_F32xF32,
    // rules.mapSeq,
    // rules.reduceSeq,
    rules.reduceSeqUnroll,
    rules.omp.mapPar,
  )

  private def goals(): () = {
    goal("baseline", apps.tvmGemm.baseline)

    // FIXME: Elevate reference seems to be broken here and there (e.g. wrong innermost traversal)
    goal("blocking", apps.tvmGemm.blocking)
    goal("vectorization", apps.tvmGemm.vectorization)
    goal("loop-perm", apps.tvmGemm.loopPerm)
    goal("array-packing", apps.tvmGemm.arrayPacking)
    goal("cache-blocks", apps.tvmGemm.cacheBlocks)
    goal("parallel", apps.tvmGemm.par)
  }

  private def M = 1024
  private def N = 1024
  private def K = 1024

  private def goal(name: String, strategy: elevate.core.Strategy[rise.core.Expr]): () = {
    val goal = (rise.elevate.strategies.normalForm.DFNF() `;` strategy) {
      import rise.core.DSL._
      mm(M)(N)(K)
    }.get
    val normGoal = BENF(Expr.fromNamed(goal))
    val goalSize = AstSize.ofExpr(normGoal)
    util.dotPrintTmp(s"${name}_goal", Expr.toNamed(normGoal))
    println(s"${name} goal size: ${goalSize}")

    val loweredGoal = lowerToC.apply(goal).get
    println(s"normalized ${name} goal (lowered): ${loweredGoal}")
    val goalCode = util.gen.openmp.function.asStringFromExpr(loweredGoal)
    util.writeToPath(s"/tmp/${name}_goal.c", goalCode)
  }

  private def codegen(name: String, e: Expr): () = {
    // val loweredWithElevate = lowerToC.apply(Expr.toNamed(e)).get
    // println(loweredWithElevate)

    // TODO: prefer hoisted lets
    object Cost extends CostFunction[Int] {
      val ordering = implicitly

      override def cost(egraph: EGraph, enode: ENode, t: TypeId, costs: EClassId => Int): Int = {
        import rise.core.primitives._
        val nodeCost = enode match {
          // prefer asVectorAligned, until we can deal with alignement better
          case Primitive(asVector()) => 1_000
          // prefer vectorized sequential maps, need to generalize this
          case Primitive(mapSeq()) =>
            val vectorizedP = {
              import ExtendedPatternDSL._

              (vecT(`?n`, `?dt`) ->: vecT(`?n`, `?dt`)) ->: `?t` ->: `?t`
            }
            if (ExtendedPattern.typeIsMatch(egraph, vectorizedP, t)) {
              1
            } else {
              2
            }
          case _ => 2
        }
        enode.children().foldLeft(nodeCost) { case (acc, eclass) => acc + costs(eclass) }
      }
    }

    LoweringSearch.init().run(BENF, Cost, Seq(e), Seq(
      // TODO:
      // rules.hoistLetApp, rules.hoistLetLam,
      rules.mapFusion,
      rules.reduceSeq,
      rules.mapSeq,
      rules.mapSeqArray,
      rules.vectorize.promoteAligned
    )) match {
      case Some(res) =>
        val loweredWithEqsat =
          elevate.core.strategies.basic.repeat(
            elevate.core.strategies.traversal.topDown(
              apps.cameraPipelineRewrite.letHoist))(
                Expr.toNamed(res)).get
        println(loweredWithEqsat)

        val withSizes = {
          // FIXME: doing this before the lowering search seems to slow it down
          import rise.eqsat.ExprDSL._
          val eg = EGraph.empty()

          def betaNat(e: ExprWithHashCons, n: Nat): ExprWithHashCons = {
            e.node match {
              case NatLambda(b) => b.withNatArgument(eg, eg.addNat(n))
              case _ => ???
            }
          }

          val e2 = betaNat(betaNat(betaNat(
            ExprWithHashCons.fromExpr(eg)(Expr.fromNamed(loweredWithEqsat)),
            cst(M)), cst(N)), cst(K))
          Expr.toNamed(ExprWithHashCons.expr(eg)(e2))
        }

        val code = util.gen.openmp.function.asStringFromExpr(withSizes)
        util.writeToPath(s"/tmp/${name.replace(' ', '_')}.c", code)
      case None => println("could not generate code")
    }
  }

  private def baseline(): GuidedSearch.Result = {
    val start = mm

    val steps = Seq(
      emptyStep withRules Seq(rules.reduceSeq, rules.reduceSeqMapFusion)
        withSketch
        containsMap(m,
          containsMap(n,
            containsReduceSeq(k, containsAddMul))),
    )

    GuidedSearch.init()
      .withFilter(StandardConstraintsPredicate)
      .run(start, steps)
  }

  val runnerTrans: Runner => Runner = r => r
    .withTimeLimit(java.time.Duration.ofMinutes(45))
    .withMemoryLimit(32L * 1024L * 1024L * 1024L)
    .withNodeLimit(50_000_000)

  private def blocking_T(tilingStep: GuidedSearch.Step): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get

    val steps = Seq(
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
      .withRunnerTransform(runnerTrans)
      .run(start, steps)
  }

  private def blocking_TTTT(tilingStep: GuidedSearch.Step): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get

    val steps = Seq(
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
      .withRunnerTransform(runnerTrans)
      .run(start, steps)
  }

  private def blocking_SRSR(splitStep: GuidedSearch.Step,
                            reorderStep: GuidedSearch.Step): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get

    val steps = Seq(
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
      .withRunnerTransform(runnerTrans)
      .run(start, steps)
  }

  private def blocking_TT(tilingStep: GuidedSearch.Step): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get

    val steps = Seq(
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
      .withRunnerTransform(runnerTrans)
      .run(start, steps)
  }

  private def blocking_SR(splitStep: GuidedSearch.Step,
                          reorderStep: GuidedSearch.Step): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get

    val steps = Seq(
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
      .withRunnerTransform(runnerTrans)
      .run(start, steps)
  }

  private def vectorization_SRL(): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get
    // could start directly from blocking outcome

    val steps = Seq(
      splitStepBENF withSketch
        containsMap(m /^ cst(32),
          containsMap(cst(32),
            containsMap(n /^ cst(32),
              containsMap(cst(32),
                containsReduceSeq(k /^ cst(4),
                  containsReduceSeq(cst(4), containsAddMul)))))),
      reorderStepBENF withSketch
        containsMap(m /^ cst(32),
          containsMap(n /^ cst(32),
            containsReduceSeq(k /^ cst(4),
              containsReduceSeq(cst(4),
                containsMap(cst(32),
                  containsMap(cst(32), containsAddMul)))))),
      loweringStep withSketch
        containsMap(m /^ cst(32),
          containsMap(n /^ cst(32),
            containsReduceSeq(k /^ cst(4),
              containsReduceSeq(cst(4),
                containsMap(cst(32),
                  containsMap(cst(1), containsAddMulVec)))))),
    )

    GuidedSearch.init()
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(300) &&
        StandardConstraintsPredicate)
      .withRunnerTransform(runnerTrans)
      .run(start, steps)
  }

  private def vectorization(): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get
    // could start directly from blocking outcome

    val steps = Seq(
      (splitStepBENF compose reorderStepBENF compose loweringStep) withSketch
        containsMap(m /^ cst(32),
          containsMap(n /^ cst(32),
            containsReduceSeq(k /^ cst(4),
              containsReduceSeq(cst(4),
                containsMap(cst(32),
                  containsMap(cst(1), containsAddMulVec)))))),
    )

    GuidedSearch.init()
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(300) &&
        StandardConstraintsPredicate)
      .withRunnerTransform(runnerTrans)
      .run(start, steps)
  }

  private def loopPerm_SRL(): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get

    val steps = Seq(
      splitStepBENF withSketch
        containsMap(m /^ cst(32),
          containsMap(cst(32),
            containsMap(n /^ cst(32),
              containsMap(cst(32),
                containsReduceSeq(k /^ cst(4),
                  containsReduceSeq(cst(4), containsAddMul)))))),
      reorderStepBENF withSketch
        containsMap(m /^ cst(32),
          containsMap(n /^ cst(32),
            containsReduceSeq(k /^ cst(4),
              containsMap(cst(32),
                containsReduceSeq(cst(4),
                  containsMap(cst(32), containsAddMul)))))),
      loweringStep withSketch
        containsMap(m /^ cst(32),
          containsMap(n /^ cst(32),
            containsReduceSeq(k /^ cst(4),
              containsMap(cst(32),
                containsReduceSeq(cst(4),
                  containsMap(cst(1), containsAddMulVec)))))),
    )

    GuidedSearch.init()
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(300) &&
        StandardConstraintsPredicate)
      .withRunnerTransform(runnerTrans)
      .run(start, steps)
  }

  private def loopPerm(): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get

    val steps = Seq(
      (splitStepBENF compose reorderStepBENF compose loweringStep) withSketch
        containsMap(m /^ cst(32),
          containsMap(n /^ cst(32),
            containsReduceSeq(k /^ cst(4),
              containsMap(cst(32),
                containsReduceSeq(cst(4),
                  containsMap(cst(1), containsAddMulVec)))))),
    )

    GuidedSearch.init()
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(300) &&
        StandardConstraintsPredicate)
      .withRunnerTransform(runnerTrans)
      .run(start, steps)
  }


  private val arrayPackingSRC = Seq(
    splitStepBENF withSketch
      containsMap(m /^ cst(32),
        containsMap(cst(32),
          containsMap(n /^ cst(32),
            containsMap(cst(32),
              containsReduceSeq(k /^ cst(4),
                containsReduceSeq(cst(4), containsAddMul)))))),
    reorderStepBENF withSketch
      containsMap(m /^ cst(32),
        containsMap(n /^ cst(32),
          containsReduceSeq(k /^ cst(4),
            containsMap(cst(32),
              containsReduceSeq(cst(4),
                containsMap(cst(32), containsAddMul)))))),
    copyStep withSketch
      containsMap(m /^ cst(32),
        containsMap(n /^ cst(32),
          containsReduceSeq(k /^ cst(4),
            containsMap(cst(32),
              containsReduceSeq(cst(4),
                containsMap(cst(32), containsAddMul))))),
        contains(app(let, app(toMem :: (`?t` ->: (n`.`(k`.`f32))),
          containsMap(n /^ cst(32),
            containsMap(k,
              containsMap(cst(32)`.`f32, ?))))))),
  )

  private def arrayPacking_SRCL(): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get

    val steps = arrayPackingSRC ++ Seq(
      loweringStep withSketch
        containsMap(m /^ cst(32),
          containsMap(n /^ cst(32),
            containsReduceSeq(k /^ cst(4),
              containsMap(cst(32),
                containsReduceSeq(cst(4),
                  containsMap(cst(1), containsAddMulVec))))),
          contains(app(let, app(toMem :: (`?t` ->: (n`.`(k`.`f32))),
            containsMapPar(n /^ cst(32),
              containsMap(k,
                containsMap(cst(1)`.`vecT(cst(32), f32), ?))))))),
    )

    GuidedSearch.init()
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(300) &&
        StandardConstraintsPredicate)
      .withRunnerTransform(runnerTrans)
      .run(start, steps)
  }

  private def arrayPacking(): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get

    val steps = Seq(
      (splitStepBENF compose reorderStepBENF compose copyStep compose loweringStep) withSketch
        containsMap(m /^ cst(32),
          containsMap(n /^ cst(32),
            containsReduceSeq(k /^ cst(4),
              containsMap(cst(32),
                containsReduceSeq(cst(4),
                  containsMap(cst(1), containsAddMulVec))))),
          contains(app(let, app(toMem :: (`?t` ->: (n`.`(k`.`f32))),
            containsMapPar(n /^ cst(32),
              containsMap(k,
                containsMap(cst(1)`.`vecT(cst(32), f32), ?))))))),
    )

    GuidedSearch.init()
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(300) &&
        StandardConstraintsPredicate)
      .withRunnerTransform(runnerTrans)
      .run(start, steps)
  }

  private def cacheBlocks_SRCL(): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get
    // val start = apps.tvmGemm.arrayPacking(mm).get

    val steps = arrayPackingSRC ++ Seq(
      loweringStep withSketch
        containsMap(m /^ cst(32),
          containsMap(n /^ cst(32),
            containsReduceSeq(k /^ cst(4),
              containsMap(cst(32),
                containsReduceSeqUnroll(cst(4),
                  containsMap(cst(1), containsAddMulVec))))),
          contains(app(let, app(toMem :: (`?t` ->: (n`.`(k`.`f32))),
            containsMapPar(n /^ cst(32),
              containsMap(k,
                containsMap(cst(1)`.`vecT(cst(32), f32), ?))))))),
    )

    GuidedSearch.init()
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(300) &&
        StandardConstraintsPredicate)
      .withRunnerTransform(runnerTrans)
      .run(start, steps)
  }

  private def cacheBlocks(): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get
    // val start = apps.tvmGemm.arrayPacking(mm).get

    val steps = Seq(
      (splitStepBENF compose reorderStepBENF compose copyStep compose loweringStep) withSketch
        containsMap(m /^ cst(32),
          containsMap(n /^ cst(32),
            containsReduceSeq(k /^ cst(4),
              containsMap(cst(32),
                containsReduceSeqUnroll(cst(4),
                  containsMap(cst(1), containsAddMulVec))))),
          contains(app(let, app(toMem :: (`?t` ->: (n`.`(k`.`f32))),
            containsMapPar(n /^ cst(32),
              containsMap(k,
                containsMap(cst(1)`.`vecT(cst(32), f32), ?))))))),
    )

    GuidedSearch.init()
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(300) &&
        StandardConstraintsPredicate)
      .withRunnerTransform(runnerTrans)
      .run(start, steps)
  }

  private def parallel_SRCL(): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get
    // val start = apps.tvmGemm.arrayPacking(mm).get

    val steps = arrayPackingSRC ++ Seq(
      loweringStep withSketch
        containsMapPar(m /^ cst(32),
          containsMap(n /^ cst(32),
            containsReduceSeq(k /^ cst(4),
              containsMap(cst(32),
                containsReduceSeqUnroll(cst(4),
                  containsMap(cst(1), containsAddMulVec))))),
          contains(app(let, app(toMem :: (`?t` ->: (n`.`(k`.`f32))),
            containsMapPar(n /^ cst(32),
              containsMap(k,
                containsMap(cst(1)`.`vecT(cst(32), f32), ?))))))),
    )

    GuidedSearch.init()
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(300) &&
        StandardConstraintsPredicate)
      .withRunnerTransform(runnerTrans)
      .run(start, steps)
  }

  private def parallel(): GuidedSearch.Result = {
    val start = apps.tvmGemm.baseline(mm).get
    // val start = apps.tvmGemm.arrayPacking(mm).get

    val steps = Seq(
      (splitStepBENF compose reorderStepBENF compose copyStep compose loweringStep) withSketch
        containsMapPar(m /^ cst(32),
          containsMap(n /^ cst(32),
            containsReduceSeq(k /^ cst(4),
              containsMap(cst(32),
                containsReduceSeqUnroll(cst(4),
                  containsMap(cst(1), containsAddMulVec))))),
          contains(app(let, app(toMem :: (`?t` ->: (n`.`(k`.`f32))),
            containsMapPar(n /^ cst(32),
              containsMap(k,
                containsMap(cst(1)`.`vecT(cst(32), f32), ?))))))),
    )

    GuidedSearch.init()
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(300) &&
        StandardConstraintsPredicate)
      .withRunnerTransform(runnerTrans)
      .run(start, steps)
  }

  def main(args: Array[String]): () = {
    val fs = Seq(
      // "baseline" -> baseline _,
      // not found after 3mn+ and 2GiB+ (700K nodes, 400K classes)
      "blocking T" -> { () => blocking_T(tilingStepBENF) },
      // "blocking TTTT" -> { () => blocking_TTTT(tilingStepBENF) },
      // "blocking SRSR" -> { () => blocking_SRSR(splitStepBENF, reorderStepBENF) },
      // FIXME: the program found has unwanted split/joins
      "blocking TT" -> { () => blocking_TT(tilingStepBENF) },
       // "blocking SR" -> { () => blocking_SR(splitStepBENF, reorderStepBENF) },
      // FIXME: cannot find goal, rewriting is stuck with the given rules
      // "blocking SR CNF" -> { () => blocking_SR(splitStepCNF, reorderStepCNF) },
       // "vectorization SRL" -> vectorization_SRL _,
       "vectorization" -> vectorization _,
       // "loop-perm SRL" -> loopPerm_SRL _,
       "loop-perm" -> loopPerm _,
       // "array-packing SRCL" -> arrayPacking_SRCL _,
       "array-packing" -> arrayPacking _,
       // "cache-blocks SRCL" -> cacheBlocks_SRCL _,
       "cache-blocks" -> cacheBlocks _,
       // "parallel SRCL" -> parallel_SRCL _,
       "parallel" -> parallel _,
    )
    val rs = fs.map { case (n, f) =>
      (n, util.time(f()))
    }
    rs.foreach { case (n, (_, r)) =>
      r.exprs.headOption.foreach(codegen(n, _))
    }
    // goals()
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
