package benchmarks.eqsat

import rise.core.{types => rct}
import rise.elevate.rules.traversal.default._
import rise.eqsat.ExtendedPatternDSL._
import rise.eqsat.PredicateDSL._
import rise.eqsat._

object mvblast {
  val s0 = 32

  val (mv, mvBlast): (rise.core.Expr, rise.core.Expr) = {
    import rise.core.DSL._
    import rise.core.DSL.Type._
    import rise.core.primitives.{let => _, _}
    import rise.core.types._
    import rise.openCL.primitives.oclReduceSeq
    import rise.openCL.DSL._

    (depFun((n: Nat, m: Nat) => fun(
      (m `.` n `.` f32) ->: (n `.` f32) ->: (m `.` f32)
    )((mat, xs) =>
      mat |> map(fun(row =>
        zip(row)(xs) |>
        map(fun(x => fst(x) * snd(x))) |>
        reduce(add)(lf32(0.0f))
      ))
    )),
    depFun((n: Nat, m: Nat) => fun(
      (m `.` n `.` f32) ->: (n `.` f32) ->: (m `.` f32)
    )((mat, xs) =>
      join o mapWorkGroup(fun(matChunk => // matChunk: s0.(n.f32 x f32)
        mapLocal(fun(x => x)) o
          // TODO: check address space
          oclReduceSeq(AddressSpace.Local)(fun((acc, next) => // next: s0.s0.f32 x s0.f32
            let (toLocal(mapLocal(fun(x => x))(snd(next)))) be (localX => // localX: s0.f32
              mapLocal(fun(x => // x: f32 x s0.f32
                // TODO: check address space
                oclReduceSeq(AddressSpace.Local)(fun((acc2, next2) => // next2: (f32 x f32)
                  acc2 + fst(next2) * snd(next2)
                ))(fst(x)) $ zip(snd(x))(localX)
              )) $ zip(acc)(fst(next)))
          ))(mapLocal(fun(x => x))(generate(fun(_ => lf32(0.0f))) :: (s0 `.` f32))) $
          zip(transpose o map(split(s0)) $ matChunk)(split(s0) $ xs)
      )) o split(s0) $ mat
    )))
  }

  def containsMap(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(map :: `?t` ->: (n`.``?dt`) ->: `?t`, f))
  def containsMap(dt: DataTypePattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(map :: `?t` ->: dt ->: `?t`, f))
  def containsMap(n: NatPattern, f: ExtendedPattern, in: ExtendedPattern): ExtendedPattern =
    contains(app(app(map :: `?t` ->: (n`.``?dt`) ->: `?t`, f), in))
  def containsMapGlobal(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(ocl.mapGlobal(0) :: `?t` ->: (n`.``?dt`) ->: `?t`, f))
  def containsMapWorkGroup(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(ocl.mapWorkGroup(0) :: `?t` ->: (n`.``?dt`) ->: `?t`, f))
  def containsMapWorkGroup(dt: DataTypePattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(ocl.mapWorkGroup(0) :: `?t` ->: dt ->: `?t`, f))
  def containsMapWorkGroup(n: NatPattern, f: ExtendedPattern, in: ExtendedPattern): ExtendedPattern =
    contains(app(app(ocl.mapWorkGroup(0) :: `?t` ->: (n`.``?dt`) ->: `?t`, f), in))
  def containsMapLocal(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(ocl.mapLocal(0) :: `?t` ->: (n`.``?dt`) ->: `?t`, f))
  def containsMapLocal(dt: DataTypePattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(ocl.mapLocal(0) :: `?t` ->: dt ->: `?t`, f))
  def containsMapLocal(n: NatPattern, f: ExtendedPattern, in: ExtendedPattern): ExtendedPattern =
    contains(app(app(ocl.mapLocal(0) :: `?t` ->: (n`.``?dt`) ->: `?t`, f), in))

  def containsMapSeq(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(mapSeq :: `?t` ->: (n`.``?dt`) ->: `?t`, f))

  def containsReduce(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(reduce :: `?t` ->: `?t` ->: (n`.``?dt`) ->: `?t`, f))
  def containsReduce(n: NatPattern, f: ExtendedPattern, in: ExtendedPattern): ExtendedPattern =
    contains(app(app(app(reduce :: `?t` ->: `?t` ->: (n`.``?dt`) ->: `?t`, f), `?`), in))
  def containsReduceSeq(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(reduceSeq :: `?t` ->: `?t` ->: (n`.``?dt`) ->: `?t`, f))
  def containsReduceSeq(n: NatPattern, f: ExtendedPattern, in: ExtendedPattern): ExtendedPattern =
    contains(app(app(app(reduceSeq :: `?t` ->: `?t` ->: (n`.``?dt`) ->: `?t`, f), `?`), in))
  def containsReduceSeq(n: NatPattern, f: ExtendedPattern, init: ExtendedPattern, in: ExtendedPattern): ExtendedPattern =
    contains(app(app(app(reduceSeq :: `?t` ->: `?t` ->: (n`.``?dt`) ->: `?t`, f), init), in))
  def containsReduceSeqUnroll(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(reduceSeqUnroll :: `?t` ->: `?t` ->: (n`.``?dt`) ->: `?t`, f))
  def containsReduceSeqLocal(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(aApp(ocl.reduceSeq, local) :: `?t` ->: `?t` ->: (n`.``?dt`) ->: `?t`, f))
  def containsReduceSeqLocal(n: NatPattern, f: ExtendedPattern, in: ExtendedPattern): ExtendedPattern =
    contains(app(app(app(aApp(ocl.reduceSeq, local) :: `?t` ->: `?t` ->: (n`.``?dt`) ->: `?t`, f), ?), in))
  def containsReduceSeqLocal(n: NatPattern, f: ExtendedPattern, init: ExtendedPattern, in: ExtendedPattern): ExtendedPattern =
    contains(app(app(app(aApp(ocl.reduceSeq, local) :: `?t` ->: `?t` ->: (n`.``?dt`) ->: `?t`, f), init), in))
  def containsReduceSeqPrivate(n: NatPattern, f: ExtendedPattern): ExtendedPattern =
    contains(app(aApp(ocl.reduceSeq, `private`) :: `?t` ->: `?t` ->: (n`.``?dt`) ->: `?t`, f))

  val m = `%n`(0)
  val n = `%n`(1)

  val containsAddMul = contains(
    (app(app(add, ?), contains(mul)) : ExtendedPattern)
  )
  val containsAddMulVec = contains(
    app(app(add, ? :: vecT(`?n`, `?dt`)), contains(mul)) :: vecT(`?n`, `?dt`)
  )

  val emptyStep = GuidedSearch.Step.init(BENF) /* withExtractor
    GuidedSearch.BeamExtractor(1, LexicographicCost(BENFRedexCount(), AstSize)) */

  val splitStep = emptyStep withRules Seq(
    rules.mapFission,
    rules.reduceSeq,
    rules.eliminateMapIdentity,
    rules.reduceSeqMapFusion,
    // rules.reduceSeqMapFission,
    // rules.undoReduceSeqForAdd, //?
    // rules.mapEtaAbstraction,
    rules.splitJoin(s0),
    // rules.splitJoin1M(32),
    rules.splitJoin2M(s0),
    rules.blockedReduce(s0),
    rules.splitBeforeMap,
  )

  val reorderStep = emptyStep withRules Seq(
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

  val copyStep = GuidedSearch.Step.init(BENF) withRules Seq(
    rules.ocl.toMem(rct.AddressSpace.Local),
    // rules.splitJoin2(32),
    rules.mapArray,
    // rules.transposeAroundMapMapF1M,
  )

  val loweringStep = GuidedSearch.Step.init(BENF) withRules Seq(
    rules.mapFusion,
    // rules.vectorize.after(32, rct.f32),
    // rules.vectorize.after(32, rct.PairType(rct.f32, rct.f32)),
    // rules.vectorize.after(32, rct.PairType(rct.f32, rct.PairType(rct.f32, rct.f32))),
    // rules.vectorize.beforeMapF32,
    // rules.vectorize.beforeMap_F32xF32,
    // rules.vectorize.beforeMap_F32x_F32xF32,
    rules.ocl.reduceSeq(rct.AddressSpace.Local),
    rules.ocl.reduceSeq2(rct.AddressSpace.Local),
    rules.ocl.mapWorkGroup(0),
    rules.ocl.mapLocal(0),
  )

  private def goals(): () = {
    goal("mv-blast", mvBlast)
  }

  private def goal(name: String, expr: rise.core.Expr): () = {
    val normGoal = BENF.normalize(Expr.fromNamed(expr))
    val goalSize = AstSize.ofExpr(normGoal)
    // util.dotPrintTmp(s"${name}_goal", Expr.toNamed(normGoal))
    println(s"${name} goal (normalized): ${Expr.toNamed(normGoal)}")
    println(s"${name} goal size: ${goalSize}")

    // val loweredGoal = lowerToC.apply(expr).get
    val goalCode = util.gen.opencl.kernel.asStringFromExpr(expr)
    util.writeToPath(s"/tmp/${name}_goal.c", goalCode)
  }

  private def codegen(name: String, e: Expr): () = {
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
      // rules.ocl.reduceSeq(rct.AddressSpace.Local),
      // rules.mapSeq,
      // rules.mapSeqArray,
      rules.vectorize.promoteAligned
    )) match {
      case Some(res) =>
        val loweredWithEqsat =
          elevate.core.strategies.basic.repeat(
            elevate.core.strategies.traversal.topDown(
              apps.cameraPipelineRewrite.letHoist))(
                Expr.toNamedUnique(res)).get
        println(loweredWithEqsat)

        val code = util.gen.opencl.kernel.asStringFromExpr(loweredWithEqsat)
        util.writeToPath(s"/tmp/${name.replace(' ', '_')}.c", code)
      case None => println("could not generate code")
    }
  }

  private def baseline(): GuidedSearch.Result = {
    val start = mv

    val steps = Seq(
      emptyStep withRules Seq(
        rules.reduceSeq, rules.reduceSeqMapFusion,
        rules.ocl.mapGlobal(0), rules.ocl.reduceSeq2(rct.AddressSpace.Private),
      ) withSketch
        containsMapGlobal(m,
          containsReduceSeqPrivate(n, containsAddMul)),
    )

    GuidedSearch.init()
      .withFilter(StandardConstraintsPredicate)
      .run(start, steps)
  }

  val runnerTrans: Runner => Runner = r => r
    .withTimeLimit(java.time.Duration.ofMinutes(5))
    .withMemoryLimit(4L * 1024L * 1024L * 1024L)
    /*
    .withTimeLimit(java.time.Duration.ofMinutes(60))
    .withMemoryLimit(32L * 1024L * 1024L * 1024L)
     */
    .withNodeLimit(50_000_000)

  private def blas(): GuidedSearch.Result = {
    val start = mv

    val steps = Seq(
      splitStep withSketch
        containsMap(m /^ cst(s0),
          containsMap(cst(s0),
            containsReduceSeq(n /^ cst(s0),
              containsReduceSeq(cst(s0), containsAddMul)))),
      reorderStep withSketch
        containsMap(m /^ cst(s0),
          containsReduceSeq(n /^ cst(s0),
            containsMap(cst(s0),
              containsReduceSeq(cst(s0), containsAddMul)))),
      copyStep withSketch
        containsMap(m /^ cst(s0),
          containsMap(`?n`, ?,
          containsReduceSeq(n /^ cst(s0),
            containsMap(cst(s0),
              containsReduceSeq(cst(s0), containsAddMul,
              // FIXME: only cst(s0)`.`f32 should go to mem, on outer loop
              contains(app(let, app(aApp(ocl.toMem, local) :: (`?t` ->: (cst(s0)`.`(f32 x f32))),
                contains(map)))))),
            containsMap(cst(s0), ?), ?))),
      loweringStep withSketch
        containsMapWorkGroup(m /^ cst(s0),
          containsMapLocal(`?n`, ?,
          containsReduceSeqLocal(n /^ cst(s0),
            containsMapLocal(cst(s0),
              containsReduceSeqLocal(cst(s0), containsAddMul,
              // FIXME: only cst(s0)`.`f32 should go to mem, on outer loop
              contains(app(let, app(aApp(ocl.toMem, local) :: (`?t` ->: (cst(s0)`.`(f32 x f32))),
                containsMapLocal(cst(s0), ?)))))),
            containsMapLocal(cst(s0), ?), ?))),
    )

    GuidedSearch.init()
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(300) &&
        StandardConstraintsPredicate)
      .withRunnerTransform(runnerTrans)
      .run(start, steps)
  }

  def main(args: Array[String]): () = {
    val fs = Seq(
      "baseline" -> baseline _,
      "blas" -> blas _,
    )
    goals()
    val rs = fs.map { case (n, f) =>
      System.gc() // hint garbage collection to get more precise memory usage statistics
      (n, util.time(f()))
    }
    rs.foreach { case (n, (_, r)) =>
      r.exprs.headOption.foreach(codegen(n, _))
    }
    rs.foreach { case (n, (t, r)) =>
      println(s"-------- $n")
      val status = if (r.exprs.nonEmpty) { "found" } else { "not found" }
      println(s"$status after ${util.prettyTime(t)}")
      r.printReport()
    }
  }
}
