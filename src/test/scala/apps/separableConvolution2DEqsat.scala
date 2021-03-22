package apps

import apps.separableConvolution2D._
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core._
import rise.core.semantics.FloatData
import rise.macros.Primitive.primitive
import rise.eqsat.Basic.proveEquiv
import rise.eqsat.rules

object separableConvolution2DEqsat {
  // TODO: use full expressions instead of proxy primitives
  @primitive object weights2d extends Primitive with Builder {
    binomialWeights2d.t
  }
  @primitive object weightsV extends Primitive with Builder {
    binomialWeightsV.t
  }
  @primitive object weightsH extends Primitive with Builder {
    binomialWeightsH.t
  }
}

class separableConvolution2DEqsat extends test_util.Tests {
  import separableConvolution2DEqsat._

  private val (separateDot, separateDotT) = {
    import rise.eqsat._
    import rise.eqsat.PatternDSL._

    val mulT = lam(app(app(mul, app(fst, %(0))), app(snd, %(0))))
    def *(p: Pattern) = app(map, p)
    val sum = app(app(reduce, add), l(FloatData(0)))
    def dot(a: Pattern, b: Pattern) = app(sum, app(*(mulT), app(app(zip, a), b)))
    val w2d = prim(weights2d.primitive)
    val wV = prim(weightsV.primitive)
    val wH = prim(weightsH.primitive)

    (Rewrite.init[DefaultAnalysisData]("separate-dot-hv",
      dot(app(join, w2d), app(join, ?("nbh"))).compile(),
      dot(wV, app(*(lam(dot(wH, %(0)))), ?("nbh")))
    ),
    Rewrite.init[DefaultAnalysisData]("separate-dot-vh",
      dot(app(join, w2d), app(join, ?("nbh"))).compile(),
      dot(wH, app(*(lam(dot(wV, %(0)))), app(transpose, ?("nbh"))))
    ))
  }

  // -- algorithmic

  test("base to factorised") {
    proveEquiv(base(weights2d), factorised(weightsV)(weightsH), Seq(separateDot))
  }

  test("base to factorised (VH)") {
    proveEquiv(base(weights2d), factorisedVH(weightsV)(weightsH), Seq(separateDotT))
  }

  test("base to scanline") {
    val * : ToBeTyped[Expr] = primitives.map
    val T: ToBeTyped[Expr] = primitives.transpose
    val J = primitives.join
    val Sh = primitives.slide(3)(1)
    val Sv = primitives.slide(3)(1)
    val Dh = dot(weightsH)
    val Dv = dot(weightsV)
    proveEquiv(
      *(Sh) >> Sv >> *(T) >> *(*(fun(nbh => dot(J(weights2d))(J(nbh))))),
      Sv >> *(T >> *(Dv) >> Sh >> *(Dh)),
      // FIXME: adding padClamp2D makes the search explode
      // base(weights2d),
      // scanline(weightsV)(weightsH),
      Seq(
      rules.eta, rules.beta, rules.removeTransposePair,
      rules.mapFusion, rules.mapFission,
      rules.slideBeforeMap, rules.mapSlideBeforeTranspose, rules.slideBeforeMapMapF,
      separateDotT
    ))
  }

  test("scanline to separated") {
    proveEquiv(scanline(weightsV)(weightsH), separated(weightsV)(weightsH), Seq(
      rules.eta, rules.beta, rules.mapFission, rules.mapFusion
    ))
  }

  // -- lowering

  test("base to baseSeq") {
    proveEquiv(base(weights2d), baseSeq(weights2d), Seq(
      rules.reduceSeqUnroll, rules.mapSeq
    ))
  }

  test("factorised to factorisedSeq") {
    proveEquiv(factorised(weightsV)(weightsH), factorisedSeq(weightsV)(weightsH), Seq(
      rules.reduceSeqUnroll, rules.mapSeq
    ))
  }

  test("separated to separatedSeq") {
    proveEquiv(separated(weightsV)(weightsH), separatedSeq(weightsV)(weightsH), Seq(
      rules.reduceSeqUnroll, rules.mapSeq, rules.toMemAfterMapSeq
    ))
  }

  test("scanline to scanlineSeq") {
    proveEquiv(scanline(weightsV)(weightsH), scanlineSeq(weightsV)(weightsH), Seq(
      rules.reduceSeqUnroll, rules.mapSeq
    ))
  }

  /* TODO: rotate values
  test("scanline to regRotSeq") {
    proveEquiv(scanline(weightsV)(weightsH), regRotSeq(weightsV)(weightsH), Seq(
      rules.reduceSeqUnroll, rules.mapSeq, rules.rotateValues, rules.iterateStream
    ))
  } */
}

