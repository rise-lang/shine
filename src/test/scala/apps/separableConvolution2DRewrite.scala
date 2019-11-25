package apps

import separableConvolution2D._

import lift.core._
import lift.core.types._
import lift.core.primitives._
import lift.core.DSL._
import lift.core.HighLevelConstructs._

import elevate.core._
import elevate.lift.rules._
import elevate.lift.rules.algorithmic._
import elevate.lift.rules.movement._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.lift.strategies.normalForm._
import elevate.lift.strategies.algorithmic._
import elevate.lift.strategies.traversal._

class separableConvolution2DRewrite extends test_util.Tests {
  private val idE: Expr = fun(x => x)
  private val idS: Strategy[Lift] = strategies.basic.id()

  private val weights2d = binomialWeights2d
  private val weightsV = binomialWeightsV
  private val weightsH = binomialWeightsH

  private val * = map
  private val T = transpose
  private val P = padClamp2D(1)
  private val Sh = slide(3)(1)
  private val Sv = slide(3)(1)
  private val Dh = dot(weightsH :: (3`.`float))
  private val Dv = dot(weightsV :: (3`.`float))

  private def ben_eq(a: Expr, b: Expr): Boolean =
    betaEtaNormalForm(a).get == betaEtaNormalForm(b).get

  private val separateDot: Strategy[Lift] = {
    case App(App(App(Reduce(), rf), init), App(App(Map(), mf), App(App(Zip(), App(Join(), w)), App(Join(), nbh))))
      if ben_eq(rf, add) && init == l(0.0f) && ben_eq(mf, mulT) && w == weights2d :: (3`.`3`.`float)
    =>
      Success(nbh |> map(dot(weightsH :: (3`.`float))) |> dot(weightsV :: (3`.`float)))
    case _ => Failure(separateDot)
  }

  private val separateDotT: Strategy[Lift] = {
    case App(App(App(Reduce(), rf), init), App(App(Map(), mf), App(App(Zip(), App(Join(), w)), App(Join(), nbh))))
      if ben_eq(rf, add) && init == l(0.0f) && ben_eq(mf, mulT) && w == weights2d :: (3`.`3`.`float)
    =>
      Success(nbh |> transpose |> map(dot(weightsV :: (3`.`float))) |> dot(weightsH :: (3`.`float)))
    case _ => Failure(separateDotT)
  }

  private def assert_ben_eq(a: Expr, b: Expr): Unit =
    if (!ben_eq(a, b)) {
      throw new Exception(s"expected structural equality:\n$a\n$b")
    }

  private def rewrite_steps(a: Expr, steps: Seq[(Strategy[Lift], Expr)]): Unit = {
    steps.foldLeft[Expr](a)({ case (e, (s, expected)) =>
      val debug = betaEtaNormalForm(e).get
      val result = s(debug).get
      assert_ben_eq(result, expected)
      result
    })
  }

  //// algorithmic

  test("base to factorise") {
    rewrite_steps(base(weights2d), Seq(
      oncetd(separateDot) -> factorised(weightsV)(weightsH)
    ))
  }

  test("base to scanline") {
    rewrite_steps(base(weights2d), Seq(
      idS
        -> (P >> *(Sh) >> Sv >> *(T) >> *(*(fun(nbh => dot(join(weights2d :: (3`.`3`.`float)))(join(nbh)))))),
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
        -> scanline(weightsV)(weightsH)
    ))
  }

  test("scanline to separated") {
    rewrite_steps(scanline(weightsV)(weightsH), Seq(
      idS
        -> (P >> Sv >> *(T >> *(Dv) >> Sh >> *(Dh))),
      repeatNTimes(2, oncetd(mapFirstFission))
        -> (P >> Sv >> *(T) >> *(*(Dv)) >> *(Sh >> *(Dh))),
      skip(1)(mapFusion)
        -> (P >> Sv >> *(T >> *(Dv)) >> *(Sh >> *(Dh))),
      idS
        -> separated(weightsV)(weightsH)
    ))
  }

  //// lowering

  test("base to baseSeq") {
    rewrite_steps(base(weights2d), Seq(
      (oncetd(specialize.reduceSeqUnroll) `;`
        repeatNTimes(2, oncetd(specialize.mapSeq)))
        -> baseSeq(weights2d)
    ))
  }

  test("factorised to factorisedSeq") {
    rewrite_steps(factorised(weightsV)(weightsH), Seq(
      (repeatNTimes(2, oncetd(specialize.reduceSeqUnroll)) `;`
        repeatNTimes(2, oncetd(specialize.mapSeq)))
        -> factorisedSeq(weightsV)(weightsH)
    ))
  }

  test("separated to separatedSeq") {
    rewrite_steps(separated(weightsV)(weightsH), Seq(
      (repeatNTimes(2, oncetd(specialize.reduceSeqUnroll)) `;`
        repeatNTimes(2, oncetd(specialize.mapSeq)) `;`
        repeatNTimes(2, skip(1)(specialize.mapSeq)))
        -> separatedSeq(weightsV)(weightsH)
    ))
  }

  test("scanline to scanlineSeq") {
    rewrite_steps(scanline(weightsV)(weightsH), Seq(
      (repeatNTimes(2, oncetd(specialize.reduceSeqUnroll)) `;`
        repeatNTimes(2, oncetd(specialize.mapSeq)) `;`
        skip(1)(specialize.mapSeq))
        -> scanlineSeq(weightsV)(weightsH)
    ))
  }

  test("scanline to regRotSeq") {
    rewrite_steps(scanline(weightsV)(weightsH), Seq(
      (repeatNTimes(2, oncetd(specialize.reduceSeqUnroll)) `;`
        oncetd(specialize.slideSeq(SlideSeq.Values, idE)) `;`
        betaEtaNormalForm `;`
        oncetd(algorithmic.slideSeqFusion) `;`
        oncetd(specialize.mapSeq))
        -> regRotSeq(weightsV)(weightsH)
    ))
  }

  // TODO
  // test("scanline to regRotPar") {
  // }
}
