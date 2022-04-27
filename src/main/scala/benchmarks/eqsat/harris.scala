package benchmarks.eqsat

import rise.core.{types => rct}
import rise.core.types.{DataType => rcdt}
import rise.eqsat._
import rise.elevate.rules.traversal.default._
import rise.eqsat.PredicateDSL._
import rise.eqsat.SketchDSL._

object harris {
  val emptyStep = GuidedSearch.Step.init(BENF)

  val shapeStep = GuidedSearch.Step.init(BENF) withRules Seq(
    rules.fstReduction, rules.sndReduction,
    rules.removeTransposePair,
    rules.slideOutsideZip, rules.slideInsideZip,
    rules.mapMapFBeforeTranspose,
    rules.mapFusion,
    rules.zipRotateLeft,
    rules.zipRotateRight,
    rules.zipSame, rules.zipSwap,
    rules.mapOutsideZip,
    rules.mapIdentityAfter,
    rules.slideBeforeMap,
    rules.dropBeforeTake, rules.dropBeforeMap, rules.takeBeforeMap,
    rules.takeBeforeDrop, rules.takeInSlide, rules.dropInSlide,
    rules.fstUnzipAsMapFst, rules.sndUnzipAsMapSnd,
    rules.slideAfter2,
  )

  val computeWithStep = GuidedSearch.Step.init(BENF) withRules Seq(
    rules.fstReduction, rules.sndReduction,
    rules.takeOutsidePair,
    rules.vectorize.asScalarOutsidePair,
    rules.mapOutsidePair,
    rules.slideOutsideZip,
    rules.mapOutsideZip,
    rules.zipSame,
    rules.mapFusion
  )

  val splitStep = GuidedSearch.Step.init(BENF) withRules Seq(
    rules.splitJoin(32),
    rules.fstReduction, rules.sndReduction,
    rules.removeTransposePair,
    rules.mapFusion,
    rules.slideBeforeSplit,
    rules.slideBeforeMap,
    rules.slideBeforeSlide,
  )

  object LoweringCost extends CostFunction[Int] {
    val ordering = implicitly

    override def cost(enode: ENode, costs: EClassId => Int): Int = {
      import rise.core.primitives._
      val nodeCost = enode match {
        // prefer non-lowered primitives
        case Primitive(mapSeq() | iterateStream()) => 10
        case _ => 1
      }
      enode.children().foldLeft(nodeCost) { case (acc, eclass) => acc + costs(eclass) }
    }
  }

  val loweringStep = GuidedSearch.Step.init(BENF) withRules Seq(
    rules.fstReduction, rules.sndReduction,
    rules.removeTransposePair,
    rules.mapFusion,
    rules.mapSeq,
    rules.reduceSeq,
    rules.iterateStream,
    rules.ocl.circularBuffer(rct.AddressSpace.Global),
    rules.ocl.circularBufferLoadFusion,
    rules.ocl.reduceSeq(rct.AddressSpace.Private),
    rules.ocl.reduceSeqUnroll,
    rules.ocl.toMem(rct.AddressSpace.Private),
    rules.ocl.mapGlobal(0),
  ) withExtractor GuidedSearch.BeamExtractor(1, LoweringCost)

  // val nf = apps.harrisCornerDetectionHalideRewrite.reducedFusedForm
  val start = apps.harrisCornerDetectionHalide.harris(1, 1).toExpr

  private def goals(): Unit = {
    val normStart = apps.harrisCornerDetectionHalideRewrite.reducedFusedForm(start).get
    println(s"normalized start (Elevate): $normStart")
    println(s"start size: ${AstSize.ofExpr(Expr.fromNamed(normStart))}")

    def g(name: String, s: elevate.core.Strategy[rise.core.Expr]): Unit = {
      val goal = s(start).get
      val normGoal = BENF.normalize(Expr.fromNamed(goal))
      println(s"normalized $name goal: ${Expr.toNamed(normGoal)}")
      println(s"goal size: ${AstSize.ofExpr(normGoal)}")
    }

    g("shape", apps.harrisCornerDetectionHalideRewrite.ocl.harrisBufferedShape.reduce(_`;`_))
    g("buffered", apps.harrisCornerDetectionHalideRewrite.ocl.harrisBuffered)
    g("buffered-par", apps.harrisCornerDetectionHalideRewrite.ocl.harrisBufferedSplitPar(32))
    // TODO: harrisBufferedVecUnalignedSplitPar(4, 32)
    // TODO: harrisBufferedVecAlignedSplitPar(4, 32)
    // TODO: harrisBufferedRegRotVecAlignedSplitPar(4, 32)
  }

  val containsAddMul = contains(app(app(add, ?), contains(mul)))
  val containsDot = contains(app(reduce, contains(add))) // containsAddMul if fused
  val containsReduceSeq = contains(app(aApp(ocl.reduceSeqUnroll, `private`), contains(add)))
  val det = (`?`: Sketch) * `?` - (`?`: Sketch) * `?`
  val trace = (`?`: Sketch) + `?`
  val containsCoarsity = contains(det - (`?`: Sketch) * trace * trace)

  val containsCoarsityToPrivate = {
    val sxx: Sketch = contains(app(aApp(ocl.toMem, `private`), app(fst, ?)))
    val sxy: Sketch = contains(app(aApp(ocl.toMem, `private`), app(fst, app(snd, ?))))
    val syy: Sketch = contains(app(aApp(ocl.toMem, `private`), app(snd, app(snd, ?))))
    val det = sxx * syy - sxy * sxy
    val trace = sxx + syy
    contains(det - (`?`: Sketch) * trace * trace)
  }

  // FIXME: this is array >= 1d
  // check ?dt != array ?
  val array1d = `?n``.``?dt`
  val array2d = `?n``.`array1d
  val array3d = `?n``.`array2d

  def gray2d(input: Sketch): Sketch =
    app(containsDot :: array3d ->: array2d, contains(input))

  def gray1d(input: Sketch): Sketch =
    app(containsDot :: array2d ->: array1d, contains(input))

  def sobel2d(input: Sketch): Sketch =
    app(containsDot :: array2d ->: array2d, contains(input))

  def sobel1d(input: Sketch): Sketch =
    app(containsDot :: array1d ->: array1d, contains(input))

  def sobel1dPaired(input: Sketch): Sketch =
    app(contains(app(map, contains(app(app(makePair,
      containsDot), containsDot)))), contains(input))

  def mul2d(a: Sketch, b: Sketch): Sketch =
    app(contains(mul) :: array2d ->: array2d, contains(app(app(zip, a), b)))

  def mul2dSame(x: Sketch): Sketch =
    app(contains(mul) :: array2d ->: array2d, contains(app(app(map, ?), x)))

  def mul1d(a: Sketch, b: Sketch): Sketch =
    app(contains(mul) :: array1d ->: array1d, contains(app(app(zip, a), b)))

  def sum3x3(input: Sketch): Sketch =
    app(contains(add) :: array2d ->: array2d, contains(input))

  def sum3(input: Sketch): Sketch =
    app(contains(add) :: array1d ->: array1d, contains(input))

  def coarsity2d(sxx: Sketch, sxy: Sketch, syy: Sketch): Sketch =
    app(containsCoarsity :: array2d ->: array2d, contains(app(app(zip, sxx), contains(app(app(zip, sxy), syy)))))

  def coarsity1d(sxx: Sketch, sxy: Sketch, syy: Sketch): Sketch =
    app(containsCoarsity :: array1d ->: array1d, contains(app(app(zip, sxx), contains(app(app(zip, sxy), syy)))))

  def slide(n: Int, m: Int): Sketch =
    nApp(nApp(SketchDSL.slide, cst(n)), cst(m))

  def lineBuffer(n: Int, load: Sketch): Sketch =
    app(nApp(nApp(aApp(ocl.circularBuffer, global), cst(n)), cst(n)), load)

  def mapPar: Sketch = ocl.mapGlobal(0)

  private def codegen(name: String, e: Expr): Unit = {
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
      rules.mapFusion,
      rules.reduceSeq,
      rules.mapSeq,
      rules.mapSeqArray,
      rules.vectorize.promoteAligned
    )) match {
      case Some(res) =>
        val hoisted = elevate.core.strategies.basic.repeat(
            elevate.core.strategies.traversal.topDown(
              apps.cameraPipelineRewrite.letHoist))(
            Expr.toNamedUnique(res)).get
        // println(hoisted)
        val lowered = CSE(hoisted)
        println(lowered)

        val code = util.gen.opencl.kernel.asStringFromExpr(lowered)
        util.writeToPath(s"/tmp/${name.replace(' ', '_')}.c", code)
      case None => println("could not generate code")
    }
  }

  private def CSE(expr: rise.core.Expr): rise.core.Expr = {
    import rise.{core => rc}
    import rise.core.{primitives => rcp}

    def rec(e: rc.Expr, m: HashMap[rc.Expr, rc.Identifier]): rc.Expr = e match {
      case rc.App(y @ rc.App(rcp.let(), v), z @ rc.Lambda(x, b)) =>
        m.get(v) match {
          case None =>
            m += (v -> x)
            rc.App(y, rc.Lambda(x, rec(b, m))(z.t))(expr.t)
          case Some(ident) =>
            val r = rc.substitute.exprInExpr(ident, `for` = x, in = b)
            rec(r, m)
        }
      case _ => e
    }

    expr match {
      case rc.Identifier(_) => expr
      case rc.Lambda(x, e) => rc.Lambda(x, CSE(e))(expr.t)
      case rc.App(y @ rc.App(rcp.let(), v), z @ rc.Lambda(x, b)) =>
        rc.App(y, rc.Lambda(x, rec(b, HashMap(v -> x)))(z.t))(expr.t)
      case rc.App(f, e) => rc.App(CSE(f), CSE(e))(expr.t)
      case rc.DepLambda(rct.NatKind, x: rct.NatIdentifier, e) => rc.DepLambda(rct.NatKind, x, CSE(e))(e.t)
      case rc.DepLambda(rct.NatKind, x: rcdt.DataTypeIdentifier, e) => rc.DepLambda(rct.DataKind, x, CSE(e))(e.t)
      case rc.DepLambda(rct.NatKind, x: rct.AddressSpaceIdentifier, e) => rc.DepLambda(rct.AddressSpaceKind, x, CSE(e))(e.t)
      case rc.DepLambda(_, _, _) => ???
      case rc.DepApp(k, f, x) => rc.DepApp(k, CSE(f), x)(expr.t)
      case rc.Literal(d) => expr
      case rc.Opaque(e, t) => ???
      case rc.TypeAnnotation(e, annotation) => ???
      case rc.TypeAssertion(e, assertion) => ???
      case primitive: rc.Primitive => primitive
    }
  }

  private def shape(): GuidedSearch.Result = {
    val steps = Seq(
      emptyStep withSketch contains {
        val g = gray2d(?)
        val ix = sobel2d(g)
        val iy = sobel2d(g)
        val sxx = sum3x3(mul2d(ix, ix))
        val sxy = sum3x3(mul2d(ix, iy))
        val syy = sum3x3(mul2d(iy, iy))
        coarsity2d(sxx, sxy, syy)
      },
      /* shapeStep withSketch contains {
        val g = gray2d(?)
        val ix = sobel2d(g)
        val iy = sobel2d(g)
        val sxx = sum3x3(mul2dSame(ix))
        val sxy = sum3x3(mul2d(ix, iy))
        val syy = sum3x3(mul2dSame(iy))
        coarsity2d(sxx, sxy, syy)
      }, */
      /* shapeStep withSketch contains {
        app(map, contains {
          val g = gray2d(?)
          val ix = sobel2d(g)
          val iy = sobel2d(g)
          val sxx = sum3(mul1d(ix, ix))
          val sxy = sum3(mul1d(ix, iy))
          val syy = sum3(mul1d(iy, iy))
          coarsity1d(sxx, sxy, syy)
        })
      }, */
      /* shapeStep withSketch contains(
        (? : Sketch) |>
        app(map, contains(gray1d(?))) |> slide(3, 1) |>
        app(map, contains(sobel1d(?))) |> slide(3, 1) |>
        app(map, contains {
          val sxx = sum3(mul1d(?, ?))
          val sxy = sum3(mul1d(?, ?))
          val syy = sum3(mul1d(?, ?))
          coarsity1d(sxx, sxy, syy)
        })
      ) */
    )

    GuidedSearch.init()
      .withFilter(StandardConstraintsPredicate)
      .withRunnerTransform(r =>
        r.withNodeLimit(1_000_000)
         // .withIterationLimit(60)
         // .withScheduler(SamplingScheduler.init().withDefaultLimit(4_000))
         .withTimeLimit(java.time.Duration.ofMinutes(2)))
      .run(start, steps)
  }

  private def buffered(): GuidedSearch.Result = {
    // TODO: include shape step?
    val start = apps.harrisCornerDetectionHalideRewrite.ocl.harrisBufferedShape.reduce(_`;`_)(
      apps.harrisCornerDetectionHalide.harris(1, 1)).get

    val steps = Seq(
      emptyStep withSketch contains(
        (? : Sketch) |>
        app(map, contains(gray1d(?))) |> slide(3, 1) |>
        app(map, contains(sobel1d(?))) |> slide(3, 1) |>
        app(map, contains {
          val sxx = sum3(mul1d(?, ?))
          val sxy = sum3(mul1d(?, ?))
          val syy = sum3(mul1d(?, ?))
          coarsity1d(sxx, sxy, syy)
        })
      ),
      computeWithStep withSketch contains(
        (? : Sketch) |>
        app(map, contains(gray1d(?))) |> slide(3, 1) |>
        app(map, contains(sobel1dPaired(?))) |> slide(3, 1) |>
        app(map, contains {
          val sxx = sum3(mul1d(?, ?))
          val sxy = sum3(mul1d(?, ?))
          val syy = sum3(mul1d(?, ?))
          coarsity1d(sxx, sxy, syy)
        })
      ),
      loweringStep withSketch contains(
        (? : Sketch) |>
        lineBuffer(3, contains(app(mapSeq, containsReduceSeq))) |>
        lineBuffer(3, contains(app(mapSeq,
          contains(app(app(makePair, containsReduceSeq), containsReduceSeq))))) |>
        app(iterateStream, contains {
          val sxx = contains(app(map, containsReduceSeq))
          val sxy = sxx
          val syy = sxx
          app(app(mapSeq, containsCoarsityToPrivate),
            app(app(zip, sxx), app(app(zip, sxy), syy)))
        })
      )
    )

    GuidedSearch.init()
      .withFilter(StandardConstraintsPredicate)
      .withRunnerTransform(r =>
        r.withTimeLimit(java.time.Duration.ofMinutes(5))
          .withMemoryLimit(4L * 1024L * 1024L * 1024L /* 4GiB */))
      .run(start, steps)
  }

  private def bufferedPar(): GuidedSearch.Result = {
    // TODO: include shape step?
    val start = apps.harrisCornerDetectionHalideRewrite.ocl.harrisBufferedShape.reduce(_`;`_)(
      apps.harrisCornerDetectionHalide.harris(1, 1)).get

    val steps = Seq(
      emptyStep withSketch contains(
        (? : Sketch) |>
        app(map, contains(gray1d(?))) |> slide(3, 1) |>
        app(map, contains(sobel1d(?))) |> slide(3, 1) |>
        app(map, contains {
          val sxx = sum3(mul1d(?, ?))
          val sxy = sum3(mul1d(?, ?))
          val syy = sum3(mul1d(?, ?))
          coarsity1d(sxx, sxy, syy)
        })
      ),
      computeWithStep withSketch contains(
        (? : Sketch) |>
        app(map, contains(gray1d(?))) |> slide(3, 1) |>
        app(map, contains(sobel1dPaired(?))) |> slide(3, 1) |>
        app(map, contains {
          val sxx = sum3(mul1d(?, ?))
          val sxy = sum3(mul1d(?, ?))
          val syy = sum3(mul1d(?, ?))
          coarsity1d(sxx, sxy, syy)
        })
      ),
      splitStep withSketch contains(
        (? : Sketch) |>
        slide(36, 32) |> app(map, contains(
          (? : Sketch) |>
          app(map, contains(gray1d(?))) |> slide(3, 1) |>
          app(map, contains(sobel1dPaired(?))) |> slide(3, 1) |>
          app(map, contains {
            val sxx = sum3(mul1d(?, ?))
            val sxy = sum3(mul1d(?, ?))
            val syy = sum3(mul1d(?, ?))
            coarsity1d(sxx, sxy, syy)
          })
        ))
      ),
      loweringStep withSketch contains(
        (? : Sketch) |>
        slide(36, 32) |> app(mapPar, contains(
          (? : Sketch) |>
          lineBuffer(3, contains(app(mapSeq, containsReduceSeq))) |>
          lineBuffer(3, contains(app(mapSeq,
            contains(app(app(makePair, containsReduceSeq), containsReduceSeq))))) |>
          app(iterateStream, contains {
            val sxx = contains(app(map, containsReduceSeq))
            val sxy = sxx
            val syy = sxx
            app(app(mapSeq, containsCoarsityToPrivate),
              app(app(zip, sxx), app(app(zip, sxy), syy)))
          })
        ))
      )
    )

    GuidedSearch.init()
      .withFilter(StandardConstraintsPredicate && ArrayDimensionPredicate(4))
      .withRunnerTransform(r =>
        r.withTimeLimit(java.time.Duration.ofMinutes(5))
          .withMemoryLimit(4L * 1024L * 1024L * 1024L /* 4GiB */))
      .run(start, steps)
  }

  def main(args: Array[String]): Unit = {
    goals()
    val fs = Seq(
      // "shape" -> shape _,
      "buffered" -> buffered _,
      "buffered-par" -> bufferedPar _,
    )
    val rs = fs.map { case (n, f) =>
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
