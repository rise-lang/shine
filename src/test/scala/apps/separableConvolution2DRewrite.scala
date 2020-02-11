package apps

import separableConvolution2D._
import rise.core._
import rise.core.primitives._
import rise.core.TypedDSL._
import rise.core.HighLevelConstructs._
import elevate.core._
import elevate.rise.rules._
import elevate.rise.rules.algorithmic._
import elevate.rise.rules.movement._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.rise.Rise
import elevate.rise.strategies.normalForm._
import elevate.rise.strategies.algorithmic._
import elevate.rise.rules.traversal._
import elevate.util.makeClosed

class separableConvolution2DRewrite extends shine.test_util.Tests {
  private val idE: Expr = fun(x => x)
  private val idS: Strategy[Rise] = strategies.basic.id()

  private val weights2d = binomialWeights2d
  private val weightsV = binomialWeightsV
  private val weightsH = binomialWeightsH

  private val * = map
  private val T = transpose
  private val P = toTDSL(padClamp2D(1))
  private val Sh = slide(3)(1)
  private val Sv = slide(3)(1)
  private val Dh = toTDSL(dot)(weightsH)
  private val Dv = toTDSL(dot)(weightsV)

  private def ben_eq(a: Expr, b: Expr): Boolean = {
    val na = BENF(a).get
    val nb = BENF(b).get
    val uab: Rise = toTDSL(na) :: nb.t
    makeClosed(uab)._1 == makeClosed(nb)._1
  }

  private val separateDot: Strategy[Rise] = {
    case App(App(App(Reduce(), rf), init), App(App(Map(), mf), App(App(Zip(), App(Join(), w)), App(Join(), nbh))))
      if ben_eq(rf, add :: rf.t) && init == toExpr(l(0.0f)) && ben_eq(mf, toTDSL(mulT) :: mf.t) && w == weights2d
    =>
      Success(typed(nbh) |> map(toTDSL(dot)(weightsH)) |> toTDSL(dot)(weightsV))
    case _ => Failure(separateDot)
  }

  private val separateDotT: Strategy[Rise] = {
    case App(App(App(Reduce(), rf), init), App(App(Map(), mf), App(App(Zip(), App(Join(), w)), App(Join(), nbh))))
      if ben_eq(rf, add :: rf.t) && init == toExpr(l(0.0f)) && ben_eq(mf, toTDSL(mulT) :: mf.t) && w == weights2d
    =>
      Success(typed(nbh) |> transpose |> map(toTDSL(dot)(weightsV)) |> toTDSL(dot)(weightsH))
    case _ => Failure(separateDotT)
  }

  private def assert_ben_eq(a: Expr, b: Expr): Unit =
    if (!ben_eq(a, b)) {
      throw new Exception(s"expected structural equality:\n$a\n$b")
    }

  private def rewrite_steps(a: Expr, steps: Seq[(Strategy[Rise], Expr)]): Unit = {
    steps.foldLeft[Expr](a)({ case (e, (s, expected)) =>
      val debug = BENF(e).get
      val result = s(debug).get
      assert_ben_eq(result, expected)
      result
    })
  }

  //// algorithmic

  test("base to factorise") {
    rewrite_steps(toTDSL(base)(weights2d), Seq(
      oncetd(separateDot) -> toTDSL(factorised)(weightsV)(weightsH)
    ))
  }

  test("base to scanline") {
    rewrite_steps(toTDSL(base)(weights2d), Seq(
      idS
        -> (P >> *(Sh) >> Sv >> *(T) >> *(*(fun(nbh => toTDSL(dot)(join(weights2d))(join(nbh)))))),
      oncetd(separateDotT)
        -> (P >> *(Sh) >> Sv >> *(T) >> *(*(T >> *(Dv) >> Dh))),
      oncetd(`*f >> S -> S >> **f`)
        -> (P >> Sv >> *(*(Sh)) >> *(T) >> *(*(T >> *(Dv) >> Dh))),
      oncetd(mapFusion)
        -> (P >> Sv >> *(*(Sh)) >> *(T >> *(T >> *(Dv) >> Dh))),
      oncetd(mapFusion)
        -> (P >> Sv >> *(*(Sh) >> T >> *(T >> *(Dv) >> Dh))),
      oncetd(`*S >> T -> T >> S >> *T`)
        -> (P >> Sv >> *(T >> Sh >> *(T) >> *(T >> *(Dv) >> Dh))),
      oncetd(mapFusion)
        -> (P >> Sv >> *(T >> Sh >> *(T >> T >> *(Dv) >> Dh))),
      oncetd(`T >> T -> `)
        -> (P >> Sv >> *(T >> Sh >> *(*(Dv) >> Dh))),
      skip(1)(mapFirstFission)
        -> (P >> Sv >> *(T >> Sh >> *(*(Dv)) >> *(Dh))),
      oncetd(`S >> **f -> *f >> S`)
        -> (P >> Sv >> *(T >> *(Dv) >> Sh >> *(Dh))),
      idS
        -> toTDSL(scanline)(weightsV)(weightsH)
    ))
  }

  test("scanline to separated") {
    rewrite_steps(toTDSL(scanline)(weightsV)(weightsH), Seq(
      idS
        -> (P >> Sv >> *(T >> *(Dv) >> Sh >> *(Dh))),
      repeatNTimes(2, oncetd(mapFirstFission))
        -> (P >> Sv >> *(T) >> *(*(Dv)) >> *(Sh >> *(Dh))),
      skip(1)(mapFusion)
        -> (P >> Sv >> *(T >> *(Dv)) >> *(Sh >> *(Dh))),
      idS
        -> toTDSL(separated)(weightsV)(weightsH)
    ))
  }

  //// lowering

  test("base to baseSeq") {
    rewrite_steps(toTDSL(base)(weights2d), Seq(
      (oncetd(lowering.reduceSeqUnroll) `;`
        repeatNTimes(2, oncetd(lowering.mapSeq)))
        -> toTDSL(baseSeq)(weights2d)
    ))
  }

  test("factorised to factorisedSeq") {
    rewrite_steps(toTDSL(factorised)(weightsV)(weightsH), Seq(
      (repeatNTimes(2, oncetd(lowering.reduceSeqUnroll)) `;`
        repeatNTimes(2, oncetd(lowering.mapSeq)))
        -> toTDSL(factorisedSeq)(weightsV)(weightsH)
    ))
  }

  test("separated to separatedSeq") {
    rewrite_steps(toTDSL(separated)(weightsV)(weightsH), Seq(
      (repeatNTimes(2, oncetd(lowering.reduceSeqUnroll)) `;`
        repeatNTimes(2, oncetd(lowering.mapSeq)) `;`
        repeatNTimes(2, skip(1)(lowering.mapSeq)))
        -> toTDSL(separatedSeq)(weightsV)(weightsH)
    ))
  }

  test("scanline to scanlineSeq") {
    rewrite_steps(toTDSL(scanline)(weightsV)(weightsH), Seq(
      (repeatNTimes(2, oncetd(lowering.reduceSeqUnroll)) `;`
        repeatNTimes(2, oncetd(lowering.mapSeq)) `;`
        skip(1)(lowering.mapSeq))
        -> toTDSL(scanlineSeq)(weightsV)(weightsH)
    ))
  }

  test("scanline to regRotSeq") {
    rewrite_steps(toTDSL(scanline)(weightsV)(weightsH), Seq(
      (repeatNTimes(2, oncetd(lowering.reduceSeqUnroll)) `;`
        oncetd(lowering.slideSeq(SlideSeq.Values, idE)) `;`
        BENF `;`
        oncetd(algorithmic.slideSeqFusion) `;`
        oncetd(lowering.mapSeq))
        -> toTDSL(regRotSeq)(weightsV)(weightsH)
    ))
  }

  // TODO
  // test("scanline to regRotPar") {
  // }
}
