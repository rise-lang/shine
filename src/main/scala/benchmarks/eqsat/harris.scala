package benchmarks.eqsat

import rise.core.{types => rct}
import rise.eqsat._
import rise.elevate.rules.traversal.default._
import rise.eqsat.PredicateDSL._
import rise.eqsat.ExtendedPatternDSL._

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
  )

  // val nf = apps.harrisCornerDetectionHalideRewrite.reducedFusedForm
  val start = apps.harrisCornerDetectionHalide.harris(1, 1).toExpr

  private def shapeGoal(): () = {
    val normStart = apps.harrisCornerDetectionHalideRewrite.reducedFusedForm(start).get
    println(s"normalized start (Elevate): $normStart")
    println(s"start size: ${AstSize.ofExpr(Expr.fromNamed(normStart))}")

    val goal = apps.harrisCornerDetectionHalideRewrite.ocl.harrisBufferedShape.reduce(_`;`_)(start).get
    val normGoal = BENF.normalize(Expr.fromNamed(goal))
    println(s"normalized goal: ${Expr.toNamed(normGoal)}")
    println(s"goal size: ${AstSize.ofExpr(normGoal)}")
  }

  val containsAddMul = contains(app(app(add, ?), contains(mul)))
  val containsDot = contains(app(reduce, contains(add))) // containsAddMul if fused
  val containsReduceSeq = contains(app(aApp(ocl.reduceSeqUnroll, `private`), contains(add)))
  val det = (`?`: ExtendedPattern) * `?` - (`?`: ExtendedPattern) * `?`
  val trace = (`?`: ExtendedPattern) + `?`
  val containsCoarsity = contains(det - (`?`: ExtendedPattern) * trace * trace)

  // FIXME: this is array >= 1d
  // check ?dt != array ?
  val array1d = `?n``.``?dt`
  val array2d = `?n``.`array1d
  val array3d = `?n``.`array2d

  def gray2d(input: ExtendedPattern): ExtendedPattern =
    app(containsDot :: array3d ->: array2d, contains(input))

  def gray1d(input: ExtendedPattern): ExtendedPattern =
    app(containsDot :: array2d ->: array1d, contains(input))

  def sobel2d(input: ExtendedPattern): ExtendedPattern =
    app(containsDot :: array2d ->: array2d, contains(input))

  def sobel1d(input: ExtendedPattern): ExtendedPattern =
    app(containsDot :: array1d ->: array1d, contains(input))

  def sobel1dPaired(input: ExtendedPattern): ExtendedPattern =
    app(contains(app(map, contains(app(app(makePair,
      containsDot), containsDot)))), contains(input))

  def mul2d(a: ExtendedPattern, b: ExtendedPattern): ExtendedPattern =
    app(contains(mul) :: array2d ->: array2d, contains(app(app(zip, a), b)))

  def mul2dSame(x: ExtendedPattern): ExtendedPattern =
    app(contains(mul) :: array2d ->: array2d, contains(app(app(map, ?), x)))

  def mul1d(a: ExtendedPattern, b: ExtendedPattern): ExtendedPattern =
    app(contains(mul) :: array1d ->: array1d, contains(app(app(zip, a), b)))

  def sum3x3(input: ExtendedPattern): ExtendedPattern =
    app(contains(add) :: array2d ->: array2d, contains(input))

  def sum3(input: ExtendedPattern): ExtendedPattern =
    app(contains(add) :: array1d ->: array1d, contains(input))

  def coarsity2d(sxx: ExtendedPattern, sxy: ExtendedPattern, syy: ExtendedPattern): ExtendedPattern =
    app(containsCoarsity :: array2d ->: array2d, contains(app(app(zip, sxx), contains(app(app(zip, sxy), syy)))))

  def coarsity1d(sxx: ExtendedPattern, sxy: ExtendedPattern, syy: ExtendedPattern): ExtendedPattern =
    app(containsCoarsity :: array1d ->: array1d, contains(app(app(zip, sxx), contains(app(app(zip, sxy), syy)))))

  def slide(n: Int, m: Int): ExtendedPattern =
    nApp(nApp(ExtendedPatternDSL.slide, cst(n)), cst(m))

  def lineBuffer(n: Int, load: ExtendedPattern): ExtendedPattern =
    app(nApp(nApp(aApp(ocl.circularBuffer, global), cst(n)), cst(n)), load)

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
        (? : ExtendedPattern) |>
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
        (? : ExtendedPattern) |>
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
        (? : ExtendedPattern) |>
        app(map, contains(gray1d(?))) |> slide(3, 1) |>
        app(map, contains(sobel1dPaired(?))) |> slide(3, 1) |>
        app(map, contains {
          val sxx = sum3(mul1d(?, ?))
          val sxy = sum3(mul1d(?, ?))
          val syy = sum3(mul1d(?, ?))
          coarsity1d(sxx, sxy, syy)
        })
      ),
      // FIXME: many maps become iterateStream, they should not: give higher cost to using that primitive?
      loweringStep withSketch contains(
        (? : ExtendedPattern) |>
        lineBuffer(3, contains(app(mapSeq, containsReduceSeq))) |>
        lineBuffer(3, contains(app(mapSeq,
          contains(app(app(makePair, containsReduceSeq), containsReduceSeq))))) |>
        app(iterateStream, contains {
          val sxx = contains(app(map, containsReduceSeq))
          val sxy = sxx
          val syy = sxx
          app(app(mapSeq, containsCoarsity), // TODO: coarsity with things toPrivate
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

  def main(args: Array[String]): () = {
    shapeGoal()
    val fs = Seq(
      // "shape" -> shape _,
      "buffered" -> buffered _,
    )
    val rs = fs.map { case (n, f) =>
      (n, util.time(f()))
    }
    rs.foreach { case (n, (t, r)) =>
      println(s"-------- $n")
      val status = if (r.exprs.nonEmpty) { "found" } else { "not found" }
      println(s"$status after ${util.prettyTime(t)}")
      r.printReport()
    }
  }
}
