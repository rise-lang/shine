package benchmarks.eqsat

import rise.core.{types => rct}
import rise.core.types.{AddressSpace, DataType => rcdt}
import rise.eqsat._
import rise.elevate.rules.lowering.lowerToC
import rise.elevate.rules.traversal.default._
import rise.eqsat.PredicateDSL._
import rise.eqsat.SketchDSL._

object gemm {
  val gemm: rise.core.DSL.ToBeTyped[rise.core.Expr] = {
    import rise.core.DSL._
    import rise.core.types._
    import rise.core.types.DataType._
    import rise.core.primitives._

    depFun((m: Nat, n: Nat, k: Nat) =>
      fun(ArrayType(m, ArrayType(k, f32)))(A =>
        fun(ArrayType(k, ArrayType(n, f32)))(B =>
          fun(ArrayType(m, ArrayType(n, f32)))(C =>
            fun(f32)(alpha =>
              fun(f32)(beta =>
                zip(A)(C) |> map(fun(rowAC =>
                  zip(B |> transpose)(snd(rowAC)) |> map(fun(colBC =>
                    zip(fst(rowAC))(fst(colBC)) |>
                      reduce(fun((acc,ab) => acc + fst(ab) * snd(ab)))(lf32(0f)) |>
                      fun(r => (alpha * r) + (beta * snd(colBC))) ))))))))))
  }

  //Same with other way to describe dot product
  val mma: rise.core.DSL.ToBeTyped[rise.core.Expr] = {
    import rise.core.DSL._
    import rise.core.types._
    import rise.core.types.DataType._
    import rise.core.primitives._

    depFun((m: Nat, n: Nat, k: Nat) =>
      fun(ArrayType(m, ArrayType(k, f32)))(A =>
        fun(ArrayType(k, ArrayType(n, f32)))(B =>
          fun(ArrayType(m, ArrayType(n, f32)))(C =>
            zip(A)(C) |> map(fun(rowAC =>
              zip(B |> transpose)(snd(rowAC)) |> map(fun(colBC =>
                zip(fst(rowAC))(fst(colBC)) |>
                  map(fun(x => fst(x) * snd(x))) |>
                  reduce(add)(snd(colBC))))))))))
//    depFun((m: Nat, n: Nat, k: Nat) =>
//      fun(ArrayType(m, ArrayType(k, f32)))(A =>
//        fun(ArrayType(k, ArrayType(n, f32)))(B =>
//          fun(ArrayType(m, ArrayType(n, f32)))(C =>
//            zip(A)(C) |> map(fun(rowAC =>
//              zip(B |> transpose)(snd(rowAC)) |> map(fun(colBC =>
//                zip(fst(rowAC))(fst(colBC)) |>
//                  reduce(fun((acc,ab) => acc + fst(ab) * snd(ab)))(snd(colBC))))))))))
  }

  //Copy pasted from mm
  def containsMap(n: NatPattern, f: Sketch): Sketch =
    contains(app(map :: `?t` ->: (n`.``?dt`) ->: `?t`, f))
  def containsMap(dt: DataTypePattern, f: Sketch): Sketch =
    contains(app(map :: `?t` ->: dt ->: `?t`, f))
  def containsMap(n: NatPattern, f: Sketch, in: Sketch): Sketch =
    contains(app(app(map :: `?t` ->: (n`.``?dt`) ->: `?t`, f), in))
  def containsMapPar(n: NatPattern, f: Sketch): Sketch =
    contains(app(omp.mapPar :: `?t` ->: (n`.``?dt`) ->: `?t`, f))
  def containsMapPar(dt: DataTypePattern, f: Sketch): Sketch =
    contains(app(omp.mapPar :: `?t` ->: dt ->: `?t`, f))
  def containsMapPar(n: NatPattern, f: Sketch, in: Sketch): Sketch =
    contains(app(app(omp.mapPar :: `?t` ->: (n`.``?dt`) ->: `?t`, f), in))

  def containsMapSeq(n: NatPattern, f: Sketch): Sketch =
    contains(app(mapSeq :: `?t` ->: (n`.``?dt`) ->: `?t`, f))

  def containsReduceSeq(n: NatPattern, f: Sketch): Sketch =
    contains(app(reduceSeq :: `?t` ->: `?t` ->: (n`.``?dt`) ->: `?t`, f))
  def containsReduceSeq(dt: DataTypePattern, f: Sketch): Sketch =
    contains(app(reduceSeq :: `?t` ->: `?t` ->: dt ->: `?t`, f))
  def containsReduceSeqUnroll(n: NatPattern, f: Sketch): Sketch =
    contains(app(reduceSeqUnroll :: `?t` ->: `?t` ->: (n`.``?dt`) ->: `?t`, f))

  val containsAddMul = contains(
    (app(app(add, ?), contains(mul)) : Sketch)/* or
    (contains(mul) >> contains(add))*/
  )

  val emptyStep = GuidedSearch.Step.init(BENF) /* withExtractor
    GuidedSearch.BeamExtractor(1, LexicographicCost(BENFRedexCount(), AstSize)) */

  private def M = 1024
  private def N = 1024
  private def K = 1024

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
              import SketchDSL._

              (vecT(`?n`, `?dt`) ->: vecT(`?n`, `?dt`)) ->: `?t` ->: `?t`
            }
            if (Sketch.typeIsMatch(egraph, vectorizedP, t)) {
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
            Expr.toNamedUnique(res)).get
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

        val code = util.gen.cuda.kernel.asStringFromExpr(withSizes)
        util.writeToPath(s"/tmp/${name.replace(' ', '_')}.cu", code)
      case None => println("could not generate code")
    }
  }




  private def baseline(): GuidedSearch.Result = {
    val start = mma

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
    .withTimeLimit(java.time.Duration.ofMinutes(5))
    .withMemoryLimit(4L * 1024L * 1024L * 1024L)
    /*
    .withTimeLimit(java.time.Duration.ofMinutes(60))
    .withMemoryLimit(32L * 1024L * 1024L * 1024L)
     */
    .withNodeLimit(50_000_000)

  def mulNP(a: NatPattern, b: NatPattern): NatPatternNode = NatPatternNode(NatMul(a, b))

  val m = `%n`(2)
  val n = `%n`(1)
  val k = `%n`(0)
  //  val mTileBlock = `%n`(3)
  //  val nTileBlock = `%n`(4)
  //  val kTileBlock = `%n`(5)
  //  val mTileWarp = `%n`(6)
  //  val nTileWarp = `%n`(7)
  //  val kTileWarp = `%n`(8)

  val mTileBlock: Long = 128
  val nTileBlock: Long = 128
  val kTileBlock: Long = 64
  val mTileWarp: Long = 32
  val nTileWarp: Long = 32
  val mTileFrag: Long = 16
  val nTileFrag: Long = 16
  val kTileFrag: Long = 16

  val aRowBlock_t: DataTypePattern = cst(mTileBlock)`.`(k`.`f32)
  val bColumnBlock_t: DataTypePattern = cst(nTileBlock)`.`(k`.`f32)
  val cRowBlock_t: DataTypePattern = cst(mTileBlock)`.`(n`.`f32)
  val aTileBlockT_t: DataTypePattern = cst(kTileBlock)`.`(cst(mTileBlock)`.`f32)
  val bTileBlock_t: DataTypePattern = cst(kTileBlock)`.`(cst(nTileBlock)`.`f32)
  val cTileBlock_t: DataTypePattern = cst(nTileBlock) `.` (cst(mTileBlock) `.` f32)
  val aTileWarp: DataTypePattern = cst(mTileWarp) `.` (cst(kTileBlock) `.` f32)
  val bTileWarp: DataTypePattern = cst(kTileBlock) `.` (cst(nTileWarp) `.` f32)
  val cTileWarp: DataTypePattern = cst(mTileWarp) `.` (cst(nTileWarp) `.` f32)
  val blockTile_t: DataTypePattern = aTileBlockT_t x bTileBlock_t
  val warpTile_t1: DataTypePattern = cst(kTileBlock)`.`((cst(mTileWarp)`.`f32) x (cst(nTileWarp)`.`f32))

//  private val initMMA =
//    containsMap(m`.`((k`.`f32) x (n`.`f32)),
//      containsMap(n`.`((k`.`f32) x f32),
//        containsReduceSeq(k`.`(f32 x f32), containsAddMul)))
//
//  private val splitBlock =
//    containsMap((m /^ cst(mTileBlock))`.`(aRowBlock_t x cRowBlock_t),
//      containsMap(cst(mTileBlock)`.`((k`.`f32) x (n`.`f32)),
//        containsMap((n /^ cst(nTileBlock))`.`(bColumnBlock_t x (cst(nTileBlock)`.`f32)),
//          containsMap(cst(nTileBlock)`.`((k`.`f32) x f32),
//            containsReduceSeq((k /^ cst(kTileBlock))`.`((cst(kTileBlock)`.`f32) x (cst(kTileBlock)`.`f32)),
//              containsReduceSeq(cst(kTileBlock)`.`(f32 x f32), containsAddMul))))))

  private val reorderBlock =
    containsMap((m /^ cst(mTileBlock))`.`(aRowBlock_t x cRowBlock_t),
      containsMap((n /^ cst(nTileBlock))`.`(bColumnBlock_t x cTileBlock_t),
        containsReduceSeq((k /^ cst(kTileBlock))`.`blockTile_t,
          containsMap(cst(mTileBlock)`.`((cst(kTileBlock)`.`f32) x (cst(nTileBlock)`.`f32)),
            containsMap(cst(nTileBlock)`.`((cst(kTileBlock)`.`f32) x f32),
              containsReduceSeq(cst(kTileBlock)`.`(f32 x f32), containsAddMul))))))

  private val reorderBlock2 =
    containsMap(mulNP(m /^ cst(mTileBlock), n /^ cst(nTileBlock))`.`((aRowBlock_t x bColumnBlock_t) x cTileBlock_t),
      containsReduceSeq((k /^ cst(kTileBlock))`.`blockTile_t,
        containsMap(cst(mTileBlock)`.`((cst(kTileBlock)`.`f32) x (cst(nTileBlock)`.`f32)),
          containsMap(cst(nTileBlock)`.`((cst(kTileBlock)`.`f32) x f32),
            containsReduceSeq(cst(kTileBlock)`.`(f32 x f32), containsAddMul)))))

  private val splitWarp =
    containsMap(mulNP(m /^ cst(mTileBlock), n /^ cst(nTileBlock))`.`((aRowBlock_t x bColumnBlock_t) x cTileBlock_t),
      containsReduceSeq((k /^ cst(kTileBlock))`.`blockTile_t,
        containsMap(cst(mTileBlock / mTileWarp)`.`((cst(mTileWarp)`.`(cst(kTileBlock)`.`f32)) x (cst(mTileWarp)`.`(cst(nTileBlock)`.`f32))),
          containsMap(cst(mTileWarp)`.`((cst(kTileBlock)`.`f32) x (cst(nTileBlock)`.`f32)),
            containsMap(cst(nTileBlock / nTileWarp)`.`((cst(nTileWarp)`.`(cst(kTileBlock)`.`f32)) x (cst(nTileWarp)`.`f32)),
              containsMap(cst(nTileWarp)`.`((cst(kTileBlock)`.`f32) x f32),
                containsReduceSeq(cst(kTileBlock / kTileFrag)`.`((cst(kTileFrag)`.`f32) x (cst(kTileBlock)`.`f32)),
                  containsReduceSeq(cst(kTileFrag)`.`(f32 x f32), containsAddMul))))))))

  private val reorderWarp =
    containsMap(mulNP(m /^ cst(mTileBlock), n /^ cst(nTileBlock))`.`((aRowBlock_t x bColumnBlock_t) x cTileBlock_t),
      containsReduceSeq((k /^ cst(kTileBlock))`.`blockTile_t,
        containsMap(cst(mTileBlock / mTileWarp)`.`(aTileWarp x (cst(mTileWarp)`.`(cst(nTileBlock)`.`f32))),
          containsMap(cst(nTileBlock / nTileWarp)`.`(bTileWarp x (cst(nTileWarp)`.`(cst(mTileWarp)`.`f32))),
            containsReduceSeq(cst(kTileBlock / kTileFrag)`.`((cst(kTileFrag) `.` (cst(mTileWarp) `.` f32)) x (cst(kTileFrag)`.`(cst(nTileWarp)`.`f32))),
              containsMap(cst(mTileWarp)`.`((cst(kTileFrag)`.`f32) x (cst(nTileWarp)`.`f32)),
                containsMap(cst(nTileWarp)`.`((cst(kTileFrag)`.`f32) x f32),
                  containsReduceSeq(cst(kTileFrag)`.`(f32 x f32), containsAddMul))))))))

  private val reorderWarp2 =
    containsMap(mulNP(m /^ cst(mTileBlock), n /^ cst(nTileBlock))`.`((aRowBlock_t x bColumnBlock_t) x cTileBlock_t),
      containsReduceSeq((k /^ cst(kTileBlock))`.`blockTile_t,
        containsMap(cst(mTileBlock / mTileWarp * nTileBlock / nTileWarp)`.`(aTileWarp x bTileWarp),
          containsReduceSeq(cst(kTileBlock / kTileFrag)`.`((cst(kTileFrag) `.` (cst(mTileWarp) `.` f32)) x (cst(kTileFrag)`.`(cst(nTileWarp)`.`f32))),
            containsMap(cst(mTileWarp)`.`((cst(kTileFrag)`.`f32) x (cst(nTileWarp)`.`f32)),
              containsMap(cst(nTileWarp)`.`((cst(kTileFrag)`.`f32) x f32),
                containsReduceSeq(cst(kTileFrag)`.`(f32 x f32), containsAddMul)))))))


  private val initMMA =
    containsMap(m`.`((k`.`f32) x (n`.`f32)),
      containsMap(n`.`((k`.`f32) x f32),
        containsReduceSeq(k`.`(f32 x f32), containsAddMul)))

  private val splitBlock =
    containsMap((m /^ cst(mTileBlock))`.`(cst(mTileBlock)`.`(k`.`f32)) x (cst(mTileBlock)`.`(n`.`f32)),
      containsMap(cst(mTileBlock)`.`((k`.`f32) x (n`.`f32)),
        containsMap(n`.`((k`.`f32) x f32),
          containsReduceSeq(k`.`(f32 x f32), containsAddMul))))

//  private val splitBlock =
//    containsMap((m /^ cst(mTileBlock))`.`(aRowBlock_t x cRowBlock_t),
//      containsMap(cst(mTileBlock)`.`((k`.`f32) x (n`.`f32)),
//        containsMap((n /^ cst(nTileBlock))`.`(bColumnBlock_t x (cst(nTileBlock)`.`f32)),
//          containsMap(cst(nTileBlock)`.`((k`.`f32) x f32),
//            containsReduceSeq((k /^ cst(kTileBlock))`.`((cst(kTileBlock)`.`f32) x (cst(kTileBlock)`.`f32)),
//              containsReduceSeq(cst(kTileBlock)`.`(f32 x f32), containsAddMul))))))

  private def fastGemm(): GuidedSearch.Result = {
    val start = mma

    val steps = Seq(
      (emptyStep withRules
        Seq(rules.reduceSeq,
          rules.reduceSeqMapFusion)) withSketch
        initMMA,

      (emptyStep withRules
        Seq(rules.mapFission,
          rules.zipSame,
          rules.reduceSeq,
          rules.eliminateMapIdentity,
          rules.reduceSeqMapFusion,
          rules.splitJoin(mTileBlock.toInt),
          //        rules.splitJoin(nTileBlock.toInt),
          //        rules.splitJoin(kTileBlock.toInt),
          // rules.splitJoin1M(32),
          rules.splitJoin2M(mTileBlock.toInt),
          //        rules.splitJoin2M(nTileBlock.toInt),
          //        rules.splitJoin2M(kTileBlock.toInt),
          rules.blockedReduce(mTileBlock.toInt),
          rules.splitBeforeMap)) withSketch
        splitBlock,

//      reorderBlock withSketch
//      reorderBlock2 withSketch
//      splitWarp withSketch
//      reorderWarp withSketch
//      reorderWarp2
    )

//    val steps = Seq(
//      emptyStep withRules Seq(rules.reduceSeq, rules.reduceSeqMapFusion)
//        withSketch
//        containsMap(m,
//          containsMap(n,
//            containsReduceSeq(k, containsAddMul))),
//    )

    GuidedSearch.init()
      .withFilter(ArrayDimensionPredicate(6) && ASTSizePredicate(200) &&
        StandardConstraintsPredicate)
      .withRunnerTransform(runnerTrans)
      .run(start, steps)
  }

  def main(args: Array[String]): () = {
    // val names = Set(args(0))
    // fs.filter { case (k, _) => names(k) }

    val fs = Seq(
//      "baseline" -> baseline _,
      "gemm" -> fastGemm _,
    )
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