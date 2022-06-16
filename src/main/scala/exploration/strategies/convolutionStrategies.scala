package exploration.strategies

import apps.separableConvolution2D.{baseSeq, binomialWeights2d, binomialWeightsH, binomialWeightsV, factorised, scanline, separated}
import elevate.core.strategies.basic.repeatNTimes
import elevate.core.strategies.traversal.topDown
import rise.elevate.rules.lowering
import rise.core._
import rise.core.DSL._
import HighLevelConstructs._
import apps.separableConvolution2D
import elevate.core.{strategies, _}
import elevate.core.LeftChoice
import rise.elevate.rules._
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.movement._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import rise.elevate.Rise
import rise.elevate.strategies.algorithmic._
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.alternative._
import rise.core.primitives._

object convolutionStrategies {

  private val idE: Expr = fun(x => x)
  private val idS: Strategy[Rise] = elevate.core.strategies.basic.id

  private val BENF = rise.elevate.strategies.normalForm.BENF()(alternative.RiseTraversable)

  private val weights2d = binomialWeights2d
  private val weightsV = binomialWeightsV
  private val weightsH = binomialWeightsH

  private val separateDot: Strategy[Rise] =
    separateDotHV(weights2d, weightsH, weightsV)

  private val separateDotT: Strategy[Rise] =
    separateDotVH(weights2d, weightsV, weightsH)

  // lowerings
  val baseSeq = (topDown(lowering.reduceSeqUnroll) `;` repeatNTimes(2)(topDown(lowering.mapSeq)))
  val factorisedSeq = (repeatNTimes(2)(topDown(lowering.reduceSeqUnroll)) `;` repeatNTimes(2)(topDown(lowering.mapSeq)))
  val separatedSeq = (repeatNTimes(2)(topDown(lowering.reduceSeqUnroll)) `;` repeatNTimes(2)(topDown(lowering.mapSeq)) `;` repeatNTimes(2)(skip(1)(lowering.mapSeq)) `;` body(argument(lowering.toMemAfterMapSeq)))
  val scanlineSeq = (repeatNTimes(2)(topDown(lowering.reduceSeqUnroll)) `;` repeatNTimes(2)(topDown(lowering.mapSeq)) `;` skip(1)(lowering.mapSeq))
  val regRotSeq = (repeatNTimes(2)(topDown(lowering.reduceSeqUnroll)) `;` topDown(lowering.mapSeq) `;` topDown(lowering.rotateValues(idE)) `;` topDown(lowering.iterateStream))

    val loweringStrategy:Strategy[Rise] = baseSeq `<+` factorisedSeq `<+` separatedSeq `<+` scanlineSeq `<+` regRotSeq

  // strategies
  val baseToFactorise = topDown(separateDot)

  val baseToScanline =
    idS `;` BENF `;`
        topDown(separateDotT) `;` BENF `;`
        topDown(`*f >> S -> S >> **f`) `;` BENF `;`
        topDown(mapFusion) `;` BENF `;`
        topDown(mapFusion) `;` BENF `;`
        topDown(`*S >> T -> T >> S >> *T`) `;` BENF `;`
        topDown(mapFusion) `;` BENF `;`
        topDown(removeTransposePair) `;` BENF `;`
        skip(1)(mapFirstFission) `;` BENF `;`
        topDown(`S >> **f -> *f >> S`) `;` BENF `;`
        idS

  val scanlineToSeparaeted =
    idS  `;` BENF `;`
      repeatNTimes(2)(topDown(mapFirstFission)) `;` BENF `;`
      skip(1)(mapFusion) `;` BENF `;`
      idS

  val baseToScanlineMapLastFission =
    idS `;` BENF `;`
      topDown(separateDotT) `;` BENF `;`
      topDown(`*f >> S -> S >> **f`) `;` BENF `;`
      topDown(mapFusion) `;` BENF `;`
      topDown(mapFusion) `;` BENF `;`
      topDown(`*S >> T -> T >> S >> *T`) `;` BENF `;`
      topDown(mapFusion) `;` BENF `;`
      topDown(removeTransposePair) `;` BENF `;`
      (repeatNTimes(3)(skip(1)(mapLastFission())) `;` BENF `;`
        repeatNTimes(2)(topDown(mapFusion))) `;` BENF `;`
      topDown(`S >> **f -> *f >> S`) `;` BENF `;`
      idS

  val scanlineToSeparatedMapLastFission =
    idS `;` BENF `;`
      skip(0)(mapLastFission()) `;` BENF `;`
      skip(1)(mapLastFission()) `;` BENF `;`
      skip(1)(mapLastFission()) `;` BENF `;`
      skip(0)(mapFusion) `;` BENF `;`
      skip(1)(mapFusion) `;` BENF `;`
      idS

  val strategies = Set[Strategy[Rise]](
    baseToFactorise,
    baseToScanline,
    scanlineToSeparaeted,
    baseToScanlineMapLastFission,
    scanlineToSeparatedMapLastFission
  )

}
