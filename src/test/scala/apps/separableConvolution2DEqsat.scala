package apps

import apps.separableConvolution2D._
import rise.core.DSL._
import rise.core.types._
import rise.eqsat.{RewriteDirected, rules}
import rise.eqsat.ProveEquiv.syntax._

class separableConvolution2DEqsat extends test_util.Tests {
  val weights2d = binomialWeights2d
  val weightsV = binomialWeightsV
  val weightsH = binomialWeightsH

  val wrapExpr = apps.separableConvolution2DCheck.wrapExpr : ToBeTyped[rise.core.Expr] => rise.core.Expr

  private val (separateDot, separateDotT) = {
    import rise.eqsat.NamedRewrite
    import rise.eqsat.NamedRewriteDSL._

    def mulT(xName: String) = lam(xName, app(app(mul, app(fst, xName)), app(snd, xName)))
    def *(p: Pattern) = app(map, p)
    val sum = app(app(reduce, add), lf32(0))
    def dot(xName: String, a: Pattern, b: Pattern) =
      app(sum, app(*(mulT(xName)), app(app(zip, a), b)))
    val w2d: Pattern = weights2d
    val wV: Pattern = weightsV
    val wH: Pattern = weightsH

    (NamedRewrite.init("separate-dot-hv",
      dot("x", app(join, w2d :: ("t": ExprType)), app(join, ("nbh": Pattern) :: ("t": ExprType)))
        -->
      dot("x2", wV, app(*(lam("y", dot("x3", wH, "y"))), "nbh"))
    ),
    NamedRewrite.init("separate-dot-vh",
      dot("x", app(join, w2d :: ("t": ExprType)), app(join, ("nbh": Pattern) :: ("t": ExprType)))
        -->
      dot("x2", wH, app(*(lam("y", dot("x3", wV, "y"))), app(transpose, "nbh")))
    ))
  }

  private val (separateDotCNF, separateDotTCNF) = {
    import rise.eqsat.NamedRewrite
    import rise.eqsat.NamedRewriteDSL._

    def mulT(xName: String) = lam(xName, app(app(mul, app(fst, xName)), app(snd, xName)))
    def *(p: Pattern) = app(map, p)
    val sum = app(app(reduce, add), lf32(0))
    def dot(xName: String, a: Pattern) =
      app(zip, a) >> *(mulT(xName)) >> sum
    val w2d: Pattern = weights2d
    val wV: Pattern = weightsV
    val wH: Pattern = weightsH

    (NamedRewrite.init("separate-dot-hv",
      ((join :: t("t")) >> dot("x", app(join :: t("t"), w2d)))
        -->
      (*(dot("xh", wH)) >> dot("xv", wV))
    ),
    NamedRewrite.init("separate-dot-vh",
      ((join :: t("t")) >> dot("x", app(join :: t("t"), w2d)))
        -->
      (transpose >> *(dot("xv", wV)) >> dot("xh", wH))
    ))
  }

  private val proveEquiv = rise.eqsat.ProveEquiv.init()

  // -- algorithmic

  test("base to factorised") {
    proveEquiv.runBENF(wrapExpr(base(weights2d)), wrapExpr(factorised(weightsV)(weightsH)),
      Seq(separateDot))
  }

  test("base to factorised (CNF)") {
    proveEquiv.runCNF(wrapExpr(base(weights2d)), wrapExpr(factorised(weightsV)(weightsH)),
      Seq(rules.combinatory.compositionElim,
        rules.combinatory.compositionAssoc1,
        rules.combinatory.compositionAssoc2,
        rules.combinatory.compositionIntro,
        separateDotCNF), Seq())
  }

  test("base to factorised (VH)") {
    proveEquiv.runBENF(wrapExpr(base(weights2d)), wrapExpr(factorisedVH(weightsV)(weightsH)),
      Seq(separateDotT))
  }

  test("base to factorised (VH, CNF)") {
    proveEquiv.runCNF(wrapExpr(base(weights2d)), wrapExpr(factorisedVH(weightsV)(weightsH)),
      Seq(rules.combinatory.compositionElim,
        rules.combinatory.compositionAssoc1,
        rules.combinatory.compositionAssoc2,
        rules.combinatory.compositionIntro,
        separateDotTCNF), Seq())
  }

  test("base to scanline") {
    proveEquiv.runBENF(wrapExpr(base(weights2d)), wrapExpr(scanline(weightsV)(weightsH)), Seq(
      rules.removeTransposePair,
      rules.mapFusion, rules.mapFission,
      rules.slideBeforeMap, rules.mapSlideBeforeTranspose, rules.slideBeforeMapMapF,
      separateDotT
    ))
  }

  test("base to scanline (CNF)") {
    proveEquiv.runCNF(wrapExpr(base(weights2d)), wrapExpr(scanline(weightsV)(weightsH)), Seq(
      rules.combinatory.compositionElim,
      rules.combinatory.compositionAssoc1,
      rules.combinatory.compositionAssoc2,
      rules.combinatory.compositionIntro,
      rules.combinatory.removeTransposePair,
      rules.combinatory.compositionLeftId,
      rules.combinatory.compositionRightId,
      rules.combinatory.mapFusion,
      rules.combinatory.mapFission,
      rules.combinatory.slideBeforeMap,
      rules.combinatory.mapSlideBeforeTranspose,
      rules.combinatory.slideBeforeMapMapF,
      separateDotTCNF
    ), Seq())
  }

  test("scanline to separated") {
    proveEquiv.runBENF(wrapExpr(scanline(weightsV)(weightsH)),
      wrapExpr(separated(weightsV)(weightsH)), Seq(
      rules.eta, rules.mapFission, rules.mapFusion
    ), Seq(RewriteDirected.BetaExtract))
  }

  test("scanline to separated (CNF)") {
    proveEquiv.runCNF(wrapExpr(scanline(weightsV)(weightsH)),
      wrapExpr(separated(weightsV)(weightsH)), Seq(
        rules.combinatory.compositionAssoc1,
        rules.combinatory.compositionAssoc2,
        rules.combinatory.mapFission,
        rules.combinatory.mapFusion
      ), Seq())
  }

  // -- lowering

  test("base to baseSeq") {
    proveEquiv.runBENF(wrapExpr(base(weights2d)), wrapExpr(baseSeq(weights2d)), Seq(
      rules.reduceSeqUnroll, rules.mapSeq
    ))
  }

  test("factorised to factorisedSeq") {
    proveEquiv.runBENF(wrapExpr(factorised(weightsV)(weightsH)),
      wrapExpr(factorisedSeq(weightsV)(weightsH)), Seq(
      rules.reduceSeqUnroll, rules.mapSeq
    ))
  }

  test("separated to separatedSeq") {
    proveEquiv.runBENF(wrapExpr(separated(weightsV)(weightsH)),
      wrapExpr(separatedSeq(weightsV)(weightsH)), Seq(
      rules.reduceSeqUnroll, rules.mapSeq, rules.toMemAfterMapSeq
    ))
  }

  test("scanline to scanlineSeq") {
    proveEquiv.runBENF(wrapExpr(scanline(weightsV)(weightsH)),
      wrapExpr(scanlineSeq(weightsV)(weightsH)), Seq(
      rules.reduceSeqUnroll, rules.mapSeq
    ))
  }

  test("scanline to regRotSeq") {
    proveEquiv.runBENF(wrapExpr(scanline(weightsV)(weightsH)),
      wrapExpr(regRotSeq(weightsV)(weightsH)), Seq(
      rules.reduceSeqUnroll, rules.mapSeq, rules.rotateValuesScalar, rules.iterateStream
    ))
  }
}

