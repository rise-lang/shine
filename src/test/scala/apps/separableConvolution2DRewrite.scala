package apps

import separableConvolution2D._
import rise.core._
import rise.core.TypedDSL._
import rise.core.HighLevelConstructs._
import elevate.core._
import rise.elevate.rules._
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.movement._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import rise.elevate.Rise
import rise.elevate.strategies.algorithmic._
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.alternative._
import rise.elevate.util.makeClosed

class separableConvolution2DRewrite extends test_util.Tests {
  private val idE: Expr = fun(x => x)
  private val idS: Strategy[Rise] = strategies.basic.id

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

  private val BENF = rise.elevate.strategies.normalForm.BENF()(alternative.RiseTraversable)

  private def ben_eq(a: Expr, b: Expr): Boolean = {
    val na = BENF(a).get
    val nb = BENF(b).get
    val uab: Rise = toTDSL(na) :: nb.t
    makeClosed(uab)._1 == makeClosed(nb)._1
  }

  private val separateDot: Strategy[Rise] =
    separateDotHV(weights2d, weightsH, weightsV)

  private val separateDotT: Strategy[Rise] =
    separateDotVH(weights2d, weightsV, weightsH)

  private def assert_ben_eq(a: Expr, b: Expr): Unit =
    if (!ben_eq(a, b)) {
      throw new Exception(s"expected structural equality:\n" +
        s"Got:\n${BENF(a).get}\nExpected:\n${BENF(b).get}")
    }

  private def rewrite_steps(a: Expr, steps: scala.collection.Seq[(Strategy[Rise], Expr)]): Unit = {
    steps.foldLeft[Expr](a)({ case (e, (s, expected)) =>
      val debug = BENF(e).get
      val result = s(debug).get
      assert_ben_eq(result, expected)
      result
    })
  }

  //// algorithmic

  test("base to factorise") {
    rewrite_steps(toTDSL(base)(weights2d), scala.collection.Seq(
      topDown(separateDot) -> toTDSL(factorised)(weightsV)(weightsH)
    ))
  }

  test("base to scanline") {
    rewrite_steps(toTDSL(base)(weights2d), scala.collection.Seq(
      idS
        -> (P >> *(Sh) >> Sv >> *(T) >> *(*(fun(nbh => toTDSL(dot)(join(weights2d))(join(nbh)))))),
      topDown(separateDotT)
        -> (P >> *(Sh) >> Sv >> *(T) >> *(*(T >> *(Dv) >> Dh))),
      topDown(`*f >> S -> S >> **f`)
        -> (P >> Sv >> *(*(Sh)) >> *(T) >> *(*(T >> *(Dv) >> Dh))),
      topDown(mapFusion)
        -> (P >> Sv >> *(*(Sh)) >> *(T >> *(T >> *(Dv) >> Dh))),
      topDown(mapFusion)
        -> (P >> Sv >> *(*(Sh) >> T >> *(T >> *(Dv) >> Dh))),
      topDown(`*S >> T -> T >> S >> *T`)
        -> (P >> Sv >> *(T >> Sh >> *(T) >> *(T >> *(Dv) >> Dh))),
      topDown(mapFusion)
        -> (P >> Sv >> *(T >> Sh >> *(T >> T >> *(Dv) >> Dh))),
      topDown(removeTransposePair)
        -> (P >> Sv >> *(T >> Sh >> *(*(Dv) >> Dh))),
      skip(1)(mapFirstFission)
        -> (P >> Sv >> *(T >> Sh >> *(*(Dv)) >> *(Dh))),
      topDown(`S >> **f -> *f >> S`)
        -> (P >> Sv >> *(T >> *(Dv) >> Sh >> *(Dh))),
      idS
        -> toTDSL(scanline)(weightsV)(weightsH)
    ))
  }

  test("scanline to separated") {
    rewrite_steps(toTDSL(scanline)(weightsV)(weightsH), scala.collection.Seq(
      idS
        -> (P >> Sv >> *(T >> *(Dv) >> Sh >> *(Dh))),
      repeatNTimes(2)(topDown(mapFirstFission))
        -> (P >> Sv >> *(T) >> *(*(Dv)) >> *(Sh >> *(Dh))),
      skip(1)(mapFusion)
        -> (P >> Sv >> *(T >> *(Dv)) >> *(Sh >> *(Dh))),
      idS
        -> toTDSL(separated)(weightsV)(weightsH)
    ))
  }

  //// lowering

  test("base to baseSeq") {
    rewrite_steps(toTDSL(base)(weights2d), scala.collection.Seq(
      (topDown(lowering.reduceSeqUnroll) `;`
        repeatNTimes(2)(topDown(lowering.mapSeq)))
        -> toTDSL(baseSeq)(weights2d)
    ))
  }

  test("factorised to factorisedSeq") {
    rewrite_steps(toTDSL(factorised)(weightsV)(weightsH), scala.collection.Seq(
      (repeatNTimes(2)(topDown(lowering.reduceSeqUnroll)) `;`
        repeatNTimes(2)(topDown(lowering.mapSeq)))
        -> toTDSL(factorisedSeq)(weightsV)(weightsH)
    ))
  }

  test("separated to separatedSeq") {
    rewrite_steps(toTDSL(separated)(weightsV)(weightsH), scala.collection.Seq(
      (repeatNTimes(2)(topDown(lowering.reduceSeqUnroll)) `;`
        repeatNTimes(2)(topDown(lowering.mapSeq)) `;`
        repeatNTimes(2)(skip(1)(lowering.mapSeq)) `;`
        body(argument(lowering.toMemAfterMapSeq)))
        -> toTDSL(separatedSeq)(weightsV)(weightsH)
    ))
  }

  test("scanline to scanlineSeq") {
    rewrite_steps(toTDSL(scanline)(weightsV)(weightsH), scala.collection.Seq(
      (repeatNTimes(2)(topDown(lowering.reduceSeqUnroll)) `;`
        repeatNTimes(2)(topDown(lowering.mapSeq)) `;`
        skip(1)(lowering.mapSeq))
        -> toTDSL(scanlineSeq)(weightsV)(weightsH)
    ))
  }

  test("scanline to regRotSeq") {
    rewrite_steps(toTDSL(scanline)(weightsV)(weightsH), scala.collection.Seq(
      (repeatNTimes(2)(topDown(lowering.reduceSeqUnroll)) `;`
        topDown(lowering.mapSeq) `;`
        topDown(lowering.rotateValues(idE)) `;`
        topDown(lowering.iterateStream))
        -> toTDSL(regRotSeq)(weightsV)(weightsH)
    ))
  }
}
